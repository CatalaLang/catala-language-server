import { useState, type ReactElement, type ReactNode } from 'react';
import type { Typ } from '../generated/catala_types';

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
};

type CompositeEditorProps = {
  items: EditorItem[];
  atomicElements?: boolean;
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

function getTabDisplayName(item: EditorItem): ReactNode {
  if (item.type.kind !== 'TArray') return item.label;
  const editorProps = (
    item.editor as {
      props: {
        testIO?: {
          value?: { value?: { kind?: string; value?: unknown[] } };
        };
      };
    }
  ).props;
  const arrayValue = editorProps?.testIO?.value?.value;
  if (arrayValue?.kind === 'Array' && arrayValue.value) {
    return `${item.key} (${arrayValue.value.length})`;
  }
  return item.label;
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

  return (
    <div className="composite-editor">
      {scalarItems.length > 0 && !hasNonScalar && (
        <div className="simple-items-vertical">
          {scalarItems.map((item) => (
            <div
              key={item.key}
              className={`simple-item-vertical ${props.atomicElements ? 'atomic-element' : ''}`}
            >
              <label className="item-label body-1">{item.label}</label>
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
              <label className="item-label body-1">{item.label}</label>
              {item.editor}
            </div>
          ))}
        </div>
      )}

      {structuralItems.map((item) => (
        <div key={item.key} className="structural-item">
          <label className="item-label body-1">{item.label}</label>
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
                {getTabDisplayName(item)}
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
          <label className="item-label body-1">{arrayItems[0].label}</label>
          {arrayItems[0].editor}
        </div>
      )}
    </div>
  );
}
