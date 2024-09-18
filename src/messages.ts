/* eslint-disable @typescript-eslint/no-explicit-any */

/* Format definitions for message exchanges between the editor 'shell' and
   the inner webview.

   Messages should be json-able objects, and conversion to-from
   other structures (e.g. ATD-defined tests) should happen elsewhere
   i.e. we just deal with cross-window message exchange here, nothing more

   As much as possible, we would like to exchange 'rich' data structures
   (e.g. maps, or anything that ATD produces) between the extension shell and
   the editor webview.

   However, while 'normal' cross-window messaging supports that use
   case, `vscode.Webview.postMessage` requires "a string or JSON-serializable object".

   Also, when exchanging messages, it means that we lose all type information
   (received messages have type `any`), which is not great
   (types are useful for messaging!).
*/

import type { ParseResults } from './testCaseEditor';

// Down messages (shell -> webview)

export type Update = {
  kind: 'update';
  parseResults: ParseResults;
};

export type DownMessage = Update;

// Up messages (webview -> shell)

// Sent by the webview when it has loaded, to request
// the initial contents from its parent shell
export type Ready = { kind: 'ready' };

// Sent when the user has modified a test
export type Edit = {
  kind: 'edit';
  tests: any /* JSON-able equivalent of TestList */;
};

export type UpMessage = Ready | Edit;
