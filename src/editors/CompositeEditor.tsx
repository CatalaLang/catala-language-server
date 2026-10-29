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
  /** The field name; also identifies the stored tab order, so keep it stable. */
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

// Display preference, shared by editors with the same array fields.
function tabOrderKey(items: EditorItem[]): string {
  return `catala.tabOrder:${items
    .map((i) => i.key)
    .sort()
    .join(',')}`;
}

function storedTabOrder(key: string): string[] {
  try {
    const order: unknown = JSON.parse(localStorage.getItem(key) ?? '[]');
    return Array.isArray(order) ? order : [];
  } catch {
    return [];
  }
}

function storeTabOrder(key: string, order: string[]): void {
  try {
    localStorage.setItem(key, JSON.stringify(order));
  } catch {
    /* not saved */
  }
}

// Unknown keys go last, in declaration order.
function sortTabs(items: EditorItem[], order: string[]): EditorItem[] {
  const rank = (key: string): number => {
    const i = order.indexOf(key);
    return i < 0 ? order.length : i;
  };
  return [...items].sort((a, b) => rank(a.key) - rank(b.key));
}

export function CompositeEditor(props: CompositeEditorProps): ReactElement {
  const scalarItems = props.items.filter((i) => categorize(i) === 'scalar');
  const structuralItems = props.items.filter(
    (i) => categorize(i) === 'structural'
  );
  const arrayItems = props.items.filter((i) => categorize(i) === 'array');

  const hasNonScalar = structuralItems.length + arrayItems.length > 0;

  const orderKey = tabOrderKey(arrayItems);
  // stored on drop only, so Esc cancels
  const [preview, setPreview] = useState<string[] | null>(null);
  const tabs = sortTabs(arrayItems, preview ?? storedTabOrder(orderKey));
  const dragged = useRef<string | null>(null);
  const dragOver = (e: React.DragEvent<HTMLElement>, target: string): void => {
    const keys = tabs.map((t) => t.key);
    const from = dragged.current;
    // the tabs may have changed mid-drag
    if (from === null || !keys.includes(from)) return;
    e.preventDefault();
    const [i, j] = [keys.indexOf(from), keys.indexOf(target)];
    const box = e.currentTarget.getBoundingClientRect();
    const middle = box.left + box.width / 2;
    // past the middle only, or uneven tabs swap back and forth
    if (i === j || (i < j ? e.clientX < middle : e.clientX > middle)) return;
    keys.splice(i, 1);
    keys.splice(j, 0, from);
    setPreview(keys);
  };
  const endDrag = (): void => {
    dragged.current = null;
    setPreview(null);
  };

  const [activeTab, setActiveTab] = useState(
    tabs.length > 0 ? tabs[0].key : ''
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

      {arrayItems.length > 1 && (
        <div className="complex-items-container">
          <div className="tabs">
            {tabs.map((item) => (
              <button
                key={item.key}
                draggable
                onDragStart={() => (dragged.current = item.key)}
                onDragOver={(e) => dragOver(e, item.key)}
                onDrop={() => preview && storeTabOrder(orderKey, preview)}
                onDragEnd={endDrag}
                className={`tab ${activeTab === item.key ? 'active' : ''}`}
                onClick={() => setActiveTab(item.key)}
              >
                <TabHeader item={item} />
              </button>
            ))}
          </div>
          <div className="tab-content">
            {arrayItems.map((item) => (
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
