let counter = 0;
export function generateId(): string {
  let mathRnd = Math.random().toString(25).slice(2);
  // let newId = Date.now().toString(25);
  mathRnd += counter++;

  return mathRnd;
}
