import {
  useEffect,
  useRef,
  useState,
  type ReactElement,
  type ReactNode,
} from 'react';
import type { PathSegment, Typ } from '../generated/catala_types';
import { wrapIdentifier } from './Identifier';
import { pathStartsWith, useReveal } from './reveal';
import { useIntl } from 'react-intl';
import { CountBadge } from './badges';

/**
 * Base for StructEditor and TestInputsEditor. Renders label/editor pairs with
 * layout heuristics based on type structure: scalar fields wrap compactly;
 * structural fields (structs, enums with struct payloads) stack full-width;
 * array fields go into tabs (or plain if there is only one).
 */
export type EditorItem = {
  key: string;
  label: ReactNode;
  type: Typ;
  editor: ReactElement;
  count?: number;
  /** Flagged on a tab header only, where the hole would be out of sight. */
  hasUnset?: boolean;
  /** A decoration next to the label, e.g. a carry mark. On a tab header the
   *  [hasUnset] sign stands in for it. */
  mark?: ReactNode;
};

type CompositeEditorProps = {
  items: EditorItem[];
  atomicElements?: boolean;
  /** Where these items live; lets a reveal request open the right tab. */
  currentPath?: PathSegment[];
};

// True if the type needs full-width rendering (not suitable for compact wrap).
// TArray is handled separately as 'array' category before this check.
function isStructural(typ: Typ): boolean {
  if (typ.kind === 'TStruct') return true;
  if (typ.kind === 'TArray') return true;
  if (typ.kind === 'TOption') return isStructural(typ.value);
  if (typ.kind === 'TEnum') {
    return Array.from(typ.value.constructors.values()).some(
      (v) => v !== null && isStructural(v.value)
    );
  }
  return false;
}

function categorize(item: EditorItem): 'scalar' | 'structural' | 'array' {
  if (item.type.kind === 'TArray') return 'array';
  if (isStructural(item.type)) return 'structural';
  return 'scalar';
}

function TabHeader({ item }: { item: EditorItem }): ReactElement {
  const intl = useIntl();
  const hint = intl.formatMessage({ id: 'tab.hasUnset' });
  return (
    <>
      <span className="tab-name">
        {wrapIdentifier(item.label)}
        {item.hasUnset && (
          <span
            className="tab-unset codicon codicon-circle-large-outline"
            role="img"
            aria-label={hint}
            title={hint}
          ></span>
        )}
      </span>{' '}
      <span className="tab-meta">
        <CountBadge items={item.count} />
        {!item.hasUnset && item.mark}
      </span>
    </>
  );
}

function usePersistentTabOrder(arrayItems: EditorItem[]) {
  const currentKeys = arrayItems.map((i) => i.key);
  const signature = [...currentKeys].sort().join('::');
  const storageKey = signature ? `tab-order:${signature}` : '';

  const readSavedOrder = (): string[] => {
    if (!storageKey) return currentKeys;
    try {
      const saved = localStorage.getItem(storageKey);
      if (saved) {
        const parsed: string[] = JSON.parse(saved);
        const valid = parsed.filter((k) => currentKeys.includes(k));
        const missing = currentKeys.filter((k) => !valid.includes(k));
        return [...valid, ...missing];
      }
    } catch {

    }
    return currentKeys;
  };

  const [tabOrder, setTabOrder] = useState<string[]>(readSavedOrder);
  const [draggedKey, setDraggedKey] = useState<string | null>(null);

  useEffect(() => {
    setTabOrder(readSavedOrder());
  }, [storageKey, arrayItems]);

  const saveOrder = (newOrder: string[]) => {
    setTabOrder(newOrder);
    if (!storageKey) return;
    try {
      localStorage.setItem(storageKey, JSON.stringify(newOrder));
    } catch  {

    }
  };

  const handleDragStart = (key: string) => setDraggedKey(key);

  const handleDragOver = (e: React.DragEvent, targetKey: string) => {
    e.preventDefault();
    if (!draggedKey || draggedKey === targetKey) return;

    const fromIdx = tabOrder.indexOf(draggedKey);
    const toIdx = tabOrder.indexOf(targetKey);
    const newOrder = [...tabOrder];
    newOrder.splice(fromIdx, 1);
    newOrder.splice(toIdx, 0, draggedKey);

    saveOrder(newOrder);
  };

  const sortedArrayItems = [...arrayItems].sort(
    (a, b) => tabOrder.indexOf(a.key) - tabOrder.indexOf(b.key)
  );

  return {
    sortedArrayItems,
    handleDragStart,
    handleDragOver,
    resetDrag: () => setDraggedKey(null),
  };
}

export function CompositeEditor(props: CompositeEditorProps): ReactElement {
  const scalarItems = props.items.filter((i) => categorize(i) === 'scalar');
  const structuralItems = props.items.filter(
    (i) => categorize(i) === 'structural'
  );
  const arrayItems = props.items.filter((i) => categorize(i) === 'array');

  const hasNonScalar = structuralItems.length + arrayItems.length > 0;

  const {
    sortedArrayItems,
    handleDragStart,
    handleDragOver,
    resetDrag,
  } = usePersistentTabOrder(arrayItems);

  const [activeTab, setActiveTab] = useState(
    arrayItems.length > 0 ? arrayItems[0].key : ''
  );

  const reveal = useReveal();
  const revealed = useRef<number | undefined>(undefined);
  useEffect(() => {
    if (
      reveal === undefined ||
      reveal.nonce === revealed.current ||
      props.currentPath === undefined ||
      !pathStartsWith(reveal.path, props.currentPath)
    )
      return;
    revealed.current = reveal.nonce;
    const next = reveal.path[props.currentPath.length];
    if (
      next?.kind === 'StructField' &&
      arrayItems.some((i) => i.key === next.value)
    )
      setActiveTab(next.value);
  }, [reveal, props.currentPath, arrayItems]);

  return (
    <div className="composite-editor">
      {scalarItems.length > 0 && !hasNonScalar && (
        <div className="simple-items-vertical">
          {scalarItems.map((item) => (
            <div
              key={item.key}
              className={`simple-item-vertical ${props.atomicElements ? 'atomic-element' : ''}`}
            >
              <label className="item-label body-1">
                {wrapIdentifier(item.label)}
                {item.mark}
              </label>
              {item.editor}
            </div>
          ))}
        </div>
      )}

      {scalarItems.length > 0 && hasNonScalar && (
        <div className="simple-items-container">
          {scalarItems.map((item) => (
            <div
              key={item.key}
              className={`simple-item ${props.atomicElements ? 'atomic-element' : ''}`}
            >
              <label className="item-label body-1">
                {wrapIdentifier(item.label)}
                {item.mark}
              </label>
              {item.editor}
            </div>
          ))}
        </div>
      )}

      {structuralItems.map((item) => (
        <div key={item.key} className="structural-item">
          <label className="item-label body-1">
            {wrapIdentifier(item.label)}
            {item.mark}
          </label>
          {item.editor}
        </div>
      ))}

      {sortedArrayItems.length > 1 && (
        <div className="complex-items-container">
          <div className="tabs">
            {sortedArrayItems.map((item) => (
              <button
                key={item.key}
                draggable
                onDragStart={() => handleDragStart(item.key)}
                onDragOver={(e) => handleDragOver(e, item.key)}
                onDragEnd={resetDrag}
                className={`tab ${activeTab === item.key ? 'active' : ''}`}
                onClick={() => setActiveTab(item.key)}
              >
                <TabHeader item={item} />
              </button>
            ))}
          </div>
          <div className="tab-content">
            {sortedArrayItems.map((item) => (
              <div
                key={item.key}
                className={`tab-panel ${activeTab === item.key ? 'active' : 'hidden'}`}
              >
                {item.editor}
              </div>
            ))}
          </div>
        </div>
      )}

      {arrayItems.length === 1 && (
        <div className="structural-item">
          <label className="item-label body-1">
            {wrapIdentifier(arrayItems[0].label)}
            {arrayItems[0].mark}
          </label>
          {arrayItems[0].editor}
        </div>
      )}
    </div>
  );
}
