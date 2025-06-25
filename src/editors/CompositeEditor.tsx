import type { ReactElement } from "react";

/**
 * Base for StructEditor and TestInputsEditor.
 * Those work in a fairly similar way (label/editor pairs
 * with display heuristics based on recursive array
 * nesting).
 * 
 * Those heuristics are:
 * - items without any list nesting are presented first
 * (arranged in a possibly 2D flexbox to use space efficiently,
 * and presented as 'cards')
 * - lists are presented in a tabbed view, one tab
 * per list (question: what if there is a single list,
 * and no other item with nested lists?)
 * - items that are not lists but contain (arbitrarily down)
 * nested lists, get one tab in the tabbed view; they
 * are shown as a recursive version of the top component
 * (first, items without list nesting, then tabbed view...)
 */
export function CompositeEditor({
    
}): ReactElement {

}