import type { ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';
import { getDefaultValue } from './defaults';
import type {
  RuntimeValue,
  RuntimeValueRaw,
  Typ,
  ValueDef,
} from './generated/test_case';
import ValueEditor, { createRuntimeValue } from './ValueEditors';
import { assertUnreachable } from './util';

type ArrayEditorProps = {
  elementType: Typ;
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

// We introspect the array type to understand whether
// there are any nested arrays down the line.
//
// If no nesting happens, then we display the array elements
// as 'cards' within a 2D layout to maximize space use --
// otherwise, we arrange elements vertically to keep horizontal
// space for further nesting.
function hasNestedArrays(typ: Typ): boolean {
  if (typ.kind === 'TArray') {
    return true;
  } else if (
    typ.kind !== 'TEnum' &&
    typ.kind !== 'TOption' &&
    typ.kind !== 'TStruct' &&
    typ.kind !== 'TTuple'
  ) {
    return false;
  } else if (typ.kind === 'TEnum') {
    return Array.from(typ.value.constructors.values())
      .filter((val) => val !== null)
      .map((val) => val?.value)
      .some(hasNestedArrays);
  } else if (typ.kind === 'TOption') {
    return hasNestedArrays(typ.value);
  } else if (typ.kind === 'TStruct') {
    return Array.from(typ.value.fields.values()).some(hasNestedArrays);
  } else if (typ.kind === 'TTuple') {
    return typ.value.some(hasNestedArrays);
  }
  assertUnreachable(typ);
}

export function ArrayEditor(props: ArrayEditorProps): ReactElement {
  const { elementType, valueDef, onValueChange } = props;
  const runtimeValue = valueDef?.value;
  const currentArray =
    runtimeValue?.value.kind === 'Array' ? runtimeValue.value.value : [];

  const updateParent = (newArray: RuntimeValue[]): void => {
    const newValueRaw: RuntimeValueRaw = { kind: 'Array', value: newArray };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  const handleAdd = (): void => {
    const newElementValue = getDefaultValue(elementType);
    // Add a unique ID attribute when creating a new element
    // (this is required by React)
    // https://react.dev/learn/rendering-lists#keeping-list-items-in-order-with-key
    const newElement: RuntimeValue = {
      ...newElementValue,
      attrs: [
        ...(newElementValue.attrs ?? []), // Preserve existing attrs if any
        {
          kind: 'Uid',
          value: String(self.crypto.randomUUID()),
        },
      ],
    };
    updateParent([...currentArray, newElement]);
  };

  const handleUpdate = (index: number, updatedElement: RuntimeValue): void => {
    const newArray = [...currentArray];
    newArray[index] = updatedElement;
    updateParent(newArray);
  };

  const handleDelete = (index: number): void => {
    const newArray = currentArray.filter((_, i) => i !== index);
    updateParent(newArray);
  };

  const handleMove = (fromIndex: number, toIndex: number): void => {
    if (
      toIndex < 0 ||
      toIndex >= currentArray.length ||
      fromIndex === toIndex
    ) {
      return;
    }
    const newArray = [...currentArray];
    const [movedItem] = newArray.splice(fromIndex, 1);
    newArray.splice(toIndex, 0, movedItem);
    updateParent(newArray);
  };

  return (
    <div className="array-editor">
      <div className="array-header">Header</div>
      <div
        className={`array-items ${hasNestedArrays(props.elementType) ? 'array-items-nested' : 'array-items-non-nested'}`}
      >
        {currentArray.map((item, index) => {
          // Find the UID attribute for the key
          const uidAttr = item.attrs?.find((attr) => attr.kind === 'Uid');
          let itemKey: string | number;
          if (uidAttr?.kind === 'Uid') {
            itemKey = uidAttr.value;
          } else {
            console.warn(
              `Array item at index ${index} is missing a UID attribute. Falling back to index key.`
            );
            itemKey = index; // Fallback to index if UID is not found
          }

          return (
            <div key={itemKey} className="array-item">
              <div className="array-item-controls">
                <button
                  className="array-move-up"
                  onClick={() => handleMove(index, index - 1)}
                  disabled={index === 0}
                  title="Move element up"
                >
                  <span className="codicon codicon-arrow-up"></span>
                </button>
                <button
                  className="array-move-down"
                  onClick={() => handleMove(index, index + 1)}
                  disabled={index === currentArray.length - 1}
                  title="Move element down"
                >
                  <span className="codicon codicon-arrow-down"></span>
                </button>
                <button
                  className="array-delete"
                  onClick={() => handleDelete(index)}
                  title="Delete element"
                >
                  <span className="codicon codicon-trash"></span>
                </button>
              </div>
              {/* Pass the element's ValueDef down */}
              <ValueEditor
                testIO={{
                  typ: elementType,
                  value: { value: item }, // Create temporary ValueDef for the element
                }}
                onValueChange={(newItemTestIo) => {
                  // newItemTestIo contains the updated ValueDef for the element
                  if (newItemTestIo.value) {
                    handleUpdate(index, newItemTestIo.value.value); // Pass the RuntimeValue up
                  }
                  // Handle case where element value becomes undefined? Maybe delete?
                }}
              />
            </div>
          );
        })}
        {/* Note that the button is a peer of the array items (last element) */}
        <button className="array-add" onClick={handleAdd}>
          <span className="codicon codicon-add"></span>
          <FormattedMessage id="arrayEditor.addElement" />
        </button>
      </div>
    </div>
  );
}
