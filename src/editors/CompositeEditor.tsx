import { useState, type ReactElement } from 'react';
import { hasNestedArrays } from './ArrayEditor';
import type { Typ } from '../generated/test_case';

/**
 * Base for StructEditor and TestInputsEditor.
 *
 * Those work in a fairly similar way (label/editor pairs
 * with display heuristics based on recursive array
 * nesting).
 *
 * Those heuristics are:
 * - Items without any list nesting are presented first
 * (arranged in a possibly 2D flexbox to use space efficiently,
 * and presented as 'cards')
 * - Lists are presented in a tabbed view, one tab
 * per list (question: what if there is a single list,
 * and no other item with nested lists? In that case we
 * should probably display it as-is, without tabs? But we need a label still.)
 * - Items that are not lists but contain (arbitrarily down)
 * nested lists, get one tab in the tabbed view; they
 * are shown as a recursive version of the top component
 * (first, items without list nesting, then tabbed view...)
 * - When there are only simple items (no lists, no items containing
 * nested lists) we arrange those vertically within a single card.
 */

export type EditorItem = {
  key: string;
  label: string;
  type: Typ;
  editor: ReactElement;
};

type CompositeEditorProps = {
  items: EditorItem[];
  atomicElements?: boolean;
};

export function CompositeEditor(props: CompositeEditorProps): ReactElement {
  // Separate items into those with and without nested arrays
  const simpleItems = props.items.filter((item) => !hasNestedArrays(item.type));
  const complexItems = props.items.filter((item) => hasNestedArrays(item.type));

  // For tabbed view of complex items
  const [activeTab, setActiveTab] = useState(
    complexItems.length > 0 ? complexItems[0].key : ''
  );

  // If there's only one complex item, display it directly without tabs
  const singleComplexItem = complexItems.length === 1;

  return (
    <div className="composite-editor">
      {/* Simple items displayed vertically when no complex items */}
      {simpleItems.length > 0 && complexItems.length === 0 && (
        <div className="simple-items-vertical">
          {simpleItems.map((item) => (
            <div
              key={item.key}
              className={`simple-item-vertical ${props.atomicElements ? 'atomic-element' : ''}`}
            >
              <div className="item-label">{item.label}</div>
              <div className="item-editor">{item.editor}</div>
            </div>
          ))}
        </div>
      )}

      {/* Simple items in flex layout when there are complex items */}
      {simpleItems.length > 0 && complexItems.length > 0 && (
        <div className="simple-items-container">
          {simpleItems.map((item) => (
            <div
              key={item.key}
              className={`simple-item ${props.atomicElements ? 'atomic-element' : ''}`}
            >
              <div className="item-label">{item.label}</div>
              <div className="item-editor">{item.editor}</div>
            </div>
          ))}
        </div>
      )}

      {/* Complex items displayed in a tabbed view */}
      {complexItems.length > 1 && (
        <div className="complex-items-container">
          <div className="tabs">
            {complexItems.map((item) => (
              <button
                key={item.key}
                className={`tab ${activeTab === item.key ? 'active' : ''}`}
                onClick={() => setActiveTab(item.key)}
              >
                {item.label}
              </button>
            ))}
          </div>
          <div className="tab-content">
            {complexItems.map((item) => (
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

      {/* Single complex item displayed directly */}
      {singleComplexItem && (
        <>
          <div className="item-label">{complexItems[0].label}</div>
          <div className="item-editor">{complexItems[0].editor}</div>
        </>
      )}
    </div>
  );
}
