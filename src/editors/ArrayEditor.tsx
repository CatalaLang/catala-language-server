import type { ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import { useState } from 'react';
import { getDefaultValue } from '../defaults';
import type {
  RuntimeValue,
  RuntimeValueRaw,
  Typ,
  ValueDef,
} from '../generated/test_case';
import ValueEditor, { createRuntimeValue } from './ValueEditors';
import { assertUnreachable } from '../util';

type ArrayEditorProps = {
  elementType: Typ;
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editorHook?: (editor: ReactElement) => ReactElement;
};

// We introspect the array type to understand whether
// there are any nested arrays down the line.
//
// If no nesting happens, then we display the array elements
// as 'cards' within a 2D layout to maximize space use --
// otherwise, we arrange elements vertically to keep horizontal
// space for further nesting.
//
// `hasNestedArrays` is exported as this introspection capability
// is also needed by CompositeEditor
export function hasNestedArrays(typ: Typ): boolean {
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

// Get a display name for a type that can be used in UI messages
export function getTypeDisplayName(
  typ: Typ,
  intl: ReturnType<typeof useIntl>
): string {
  switch (typ.kind) {
    case 'TBool':
      return intl.formatMessage({ id: 'type.boolean' });
    case 'TInt':
      return intl.formatMessage({ id: 'type.integer' });
    case 'TRat':
      return intl.formatMessage({ id: 'type.decimal' });
    case 'TMoney':
      return intl.formatMessage({ id: 'type.money' });
    case 'TDate':
      return intl.formatMessage({ id: 'type.date' });
    case 'TDuration':
      return intl.formatMessage({ id: 'type.duration' });
    case 'TArray': {
      const baseType = getTypeDisplayName(typ.value, intl);
      return intl.formatMessage({ id: 'type.array' }, { baseType });
    }
    case 'TOption': {
      const baseType = getTypeDisplayName(typ.value, intl);
      return intl.formatMessage({ id: 'type.optional' }, { baseType });
    }
    case 'TTuple':
      return intl.formatMessage({ id: 'type.tuple' });
    case 'TStruct': {
      // For structs, extract just the name part without the package
      const fullName = typ.value.struct_name;
      const nameParts = fullName.split('.');
      return nameParts[nameParts.length - 1];
    }
    case 'TEnum': {
      // For enums, extract just the name part without the package
      const fullName = typ.value.enum_name;
      const nameParts = fullName.split('.');
      return nameParts[nameParts.length - 1];
    }
    default:
      assertUnreachable(typ);
  }
}

export function ArrayEditor(props: ArrayEditorProps): ReactElement {
  const { elementType, valueDef, onValueChange, editorHook } = props;
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

  const intl = useIntl();
  const elementTypeName = getTypeDisplayName(elementType, intl);

  // State for drag and drop
  const [draggedIndex, setDraggedIndex] = useState<number | null>(null);
  const [dragOverIndex, setDragOverIndex] = useState<number | null>(null);

  // Handle drag start
  const handleDragStart = (e: React.DragEvent, index: number): void => {
    // Check if the drag started from the controls area but not from a button
    const target = e.target as HTMLElement;
    if (target.tagName === 'BUTTON' || target.closest('button')) {
      e.preventDefault();
      return;
    }
    setDraggedIndex(index);
    // Set a ghost drag image
    if (e.dataTransfer) {
      const dragElement = document.createElement('div');
      dragElement.textContent = `Item ${index + 1}`;
      dragElement.className = 'drag-ghost';
      document.body.appendChild(dragElement);
      e.dataTransfer.setDragImage(dragElement, 0, 0);
      setTimeout(() => {
        document.body.removeChild(dragElement);
      }, 0);
    }
  };

  // Handle drag over
  const handleDragOver = (e: React.DragEvent, index: number): void => {
    e.preventDefault();
    if (draggedIndex !== null && draggedIndex !== index) {
      setDragOverIndex(index);
    }
  };

  // Handle drop
  const handleDrop = (e: React.DragEvent, index: number): void => {
    e.preventDefault();
    if (draggedIndex !== null && draggedIndex !== index) {
      handleMove(draggedIndex, index);
      setDraggedIndex(null);
      setDragOverIndex(null);
    }
  };

  // Handle drag end
  const handleDragEnd = (): void => {
    setDraggedIndex(null);
    setDragOverIndex(null);
  };

  return (
    <div className="array-editor">
      <button className="array-add" onClick={handleAdd}>
        <span className="codicon codicon-add"></span>
        <FormattedMessage
          id="arrayEditor.addElement"
          values={{ elementType: elementTypeName }}
        />
      </button>

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
            <div
              key={itemKey}
              className={`array-item ${
                dragOverIndex === index ? 'drag-over' : ''
              } ${draggedIndex === index ? 'dragging' : ''}`}
              onDragOver={(e) => handleDragOver(e, index)}
              onDrop={(e) => handleDrop(e, index)}
              onDragEnd={handleDragEnd}
            >
              <div
                className="array-item-controls"
                draggable
                onDragStart={(e) => handleDragStart(e, index)}
              >
                {hasNestedArrays(elementType) ? (
                  <>
                    <button
                      className="array-move-prev"
                      onClick={() => handleMove(index, index - 1)}
                      disabled={index === 0}
                      title={intl.formatMessage({
                        id: 'arrayEditor.movePrevious',
                      })}
                    >
                      <span className="codicon codicon-arrow-up"></span>
                    </button>
                    <button
                      className="array-move-next"
                      onClick={() => handleMove(index, index + 1)}
                      disabled={index === currentArray.length - 1}
                      title={intl.formatMessage({ id: 'arrayEditor.moveNext' })}
                    >
                      <span className="codicon codicon-arrow-down"></span>
                    </button>
                  </>
                ) : (
                  <>
                    <button
                      className="array-move-prev"
                      onClick={() => handleMove(index, index - 1)}
                      disabled={index === 0}
                      title={intl.formatMessage({
                        id: 'arrayEditor.movePrevious',
                      })}
                    >
                      <span className="codicon codicon-arrow-left"></span>
                    </button>
                    <button
                      className="array-move-next"
                      onClick={() => handleMove(index, index + 1)}
                      disabled={index === currentArray.length - 1}
                      title={intl.formatMessage({ id: 'arrayEditor.moveNext' })}
                    >
                      <span className="codicon codicon-arrow-right"></span>
                    </button>
                  </>
                )}
                <button
                  className="array-delete"
                  onClick={() => handleDelete(index)}
                  title={intl.formatMessage({
                    id: 'arrayEditor.deleteElement',
                  })}
                >
                  <span className="codicon codicon-trash"></span>
                </button>
              </div>
              <div className="array-item-content">
                {/* We only show the element type and index for complex
                    (nested subarrays) elements, because they are laid
                    out vertically so a reminder of the type and index
                    helps
                */}
                {hasNestedArrays(elementType) && (
                  <div className="array-item-header">
                    {getTypeDisplayName(elementType, intl)}
                  </div>
                )}
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
                  editorHook={editorHook}
                />
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
}
