---
    // The validator creates diagnostics for all uppercase words length 2 and more
    const text = textDocument.getText();
    const pattern = /\b[A-Z]{2,}\b/g;
    let m: RegExpExecArray | null;

    let problems = 0;

    while ((m = pattern.exec(text))) {
      problems++;
      const diagnostic: Diagnostic = {
        severity: DiagnosticSeverity.Warning,
        range: {
          start: textDocument.positionAt(m.index),
          end: textDocument.positionAt(m.index + m[0].length),
        },
        message: `${m[0]} is all uppercase.`,
        source: 'ex',
      };

      diagnostics.push(diagnostic);
    }
    return diagnostics;
---

    if (hasDiagnosticRelatedInformationCapability) {
      diagnostic.relatedInformation = [
        {
          location: {
            uri: textDocument.uri,
            range: Object.assign({}, diagnostic.range),
          },
          message: 'Spelling matters',
        },
        {
          location: {
            uri: textDocument.uri,
            range: Object.assign({}, diagnostic.range),
          },
          message: 'Particularly for names',
        },
      ];
    }

---

```c
TSNode root_node = ts_tree_root_node(tree);
TSTreeCursor cursor = ts_tree_cursor_new(root_node);
do {
  if (strcmp(ts_tree_cursor_current_node_type(cursor), "ERROR") == 0) {
    uint32_t start_byte = ts_node_start_byte(ts_tree_cursor_current_node(cursor));
    uint32_t end_byte = ts_node_end_byte(ts_tree_cursor_current_node(cursor));
    TSPoint start_point = ts_node_start_point(ts_tree_cursor_current_node(cursor));
    TSPoint end_point = ts_node_end_point(ts_tree_cursor_current_node(cursor));

    // Based on the type and location of the error, provide a custom error message.
    printf("Syntax error from byte %u to byte %u (line %u column %u to line %u column %u)\n",
           start_byte, end_byte,
           start_point.row, start_point.column,
           end_point.row, end_point.column);
  }
} while (ts_tree_cursor_goto_next_sibling(&cursor));
ts_tree_cursor_delete(&cursor);
```
---
function getWordAtPosition(document: TextDocument, position: Position) {
  const editorLine = document.getText({
    start: { line: position.line, character: 0 },
    end: { line: position.line, character: Number.MAX_VALUE },
  });

  // Extract the word at the position
  const leftPart = editorLine.slice(0, position.character + 1);
  const rightPart = editorLine.slice(position.character + 1);
  const leftMatch = leftPart.match(/\w+$/);
  const rightMatch = rightPart.match(/^\w+/);
  const word = leftMatch && rightMatch ? leftMatch[0] + rightMatch[0] : null;

  return word;
}

---
  // declaration_structure
  if (node?.type === 'struct_decl') {
    // node.descendantsOfType('enum_struct_name');
    const nameNode = findFirstChildByType(node, 'enum_struct_name');
    if (!nameNode) return;

    const structName = nameNode.text;
    const structId = nameNode.id;
    const declaration: CatalaDeclaration = {
      type: DeclarationType.Struct,
      text: structName,
      id: structId,
      startPosition: nameNode.startPosition,
      endPosition: nameNode.endPosition,
    };

    const structFieldNode = findManyChildrenByType(node, 'struct_decl_item');
    if (structFieldNode?.length > 1) return;

    const fieldNodes = findManyChildrenByType(structFieldNode[0], 'field_name');

    const fields = fieldNodes.map((fieldNode) => {
      const typeNode = findNextSiblingByType(fieldNode, 'typ');
      const primitiveTypNode = findFirstChildUntilType(
        typeNode,
        'primitive_typ'
      );

      const field: CatalaStructureField = {
        id: fieldNode.id,
        text: fieldNode.text,
        type: primitiveTypNode?.firstChild?.text ?? UNKNOWN.fieldType,
        structId,
      };
      return field;
    });
    catalaFile.fields.push(...fields);
    catalaFile.declarations.push(declaration);
  }

  // declaration_scope
  if (node?.type === 'scope_decl') {
    const nameNode = node.descendantsOfType('scope_name')[0]; // [0] Assume only one name
    //  findFirstChildByType(node, 'scope_name'); /*?*/
    if (!nameNode) return;

    const scopeId = nameNode.id;
    const scopeName = nameNode.text;
    const declaration: CatalaDeclaration = {
      type: DeclarationType.Scope,
      text: scopeName,
      id: scopeId,
      startPosition: nameNode.startPosition,
      endPosition: nameNode.endPosition,
    };

    const structFieldNode = findManyChildrenByType(node, 'scope_decl_item');
    if (structFieldNode?.length > 1) return;

    const fieldNodes = findManyChildrenByType(structFieldNode[0], 'variable');

    const fields = fieldNodes.map((fieldNode) => {
      const typeNode = findNextSiblingByType(fieldNode, 'typ');
      const primitiveTypNode = findFirstChildUntilType(
        typeNode,
        'primitive_typ'
      );

      const field: CatalaStructureField = {
        text: fieldNode.text,
        type: primitiveTypNode?.firstChild?.text ?? UNKNOWN.fieldType,
        id: fieldNode.id,
        scopeId,
      };
      return field;
    });
    catalaFile.fields.push(...fields);
    catalaFile.declarations.push(declaration);
  }
---
  // tree.rootNode.type; /*?*/
  // const cursor = tree.rootNode.walk();

  // cursor.gotoFirstChild(); /*?*/
  // cursor.currentNode.type; /*?*/
  // if (!cursor.currentNode.type) return catalaFile;

  // cursor.gotoNextSibling(); /*?*/
  // cursor.gotoFirstChild(); /*?*/
  // cursor.currentNode.type; /*?*/
