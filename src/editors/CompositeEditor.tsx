import {
  useEffect,
  useRef,
  useState,
  type ReactElement,
  type ReactNode,
} from 'react';
import type { PathSegment, Typ } from '../generated/catala_types';
import { pathStartsWith, useReveal } from './reveal';
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
  unfilled?: number;
  /** A decoration next to the label, e.g. a carry mark. On a tab header a
   *  non-zero [unfilled] count stands in for it. */
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
  const unfilled = item.unfilled ?? 0;
  return (
    <>
      <span className="tab-name">{item.label}</span>{' '}
      <span className="tab-meta">
        <CountBadge items={item.count} unfilled={unfilled} />
        {unfilled === 0 && item.mark}
      </span>
    </>
  );
}

export function CompositeEditor(props: CompositeEditorProps): ReactElement {
  const scalarItems = props.items.filter((i) => categorize(i) === 'scalar');
  const structuralItems = props.items.filter(
    (i) => categorize(i) === 'structural'
  );
  const arrayItems = props.items.filter((i) => categorize(i) === 'array');

  const hasNonScalar = structuralItems.length + arrayItems.length > 0;

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
                {item.label}
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
                {item.label}
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
            {item.label}
            {item.mark}
          </label>
          {item.editor}
        </div>
      ))}

      {arrayItems.length > 1 && (
        <div className="complex-items-container">
          <div className="tabs">
            {arrayItems.map((item) => (
              <button
                key={item.key}
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
            {arrayItems[0].label}
            {arrayItems[0].mark}
          </label>
          {arrayItems[0].editor}
        </div>
      )}
    </div>
  );
}
