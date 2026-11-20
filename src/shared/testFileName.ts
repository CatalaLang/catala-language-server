/** Whether a base name matches a custom editor's `filenamePattern`: `*` for
 * any run of characters, case ignored. */
export function matchesFilenamePattern(
  fileName: string,
  pattern: string
): boolean {
  const regex = pattern
    .split('*')
    .map((part) => part.replace(/[.+?^${}()|[\]\\]/g, '\\$&'))
    .join('.*');
  return new RegExp(`^${regex}$`, 'i').test(fileName);
}

/** The Catala language most files of the project are written in. */
export function mostCommonLanguage(
  fileNames: string[]
): 'fr' | 'en' | 'pl' | undefined {
  const counts = new Map<'fr' | 'en' | 'pl', number>();
  for (const name of fileNames) {
    const lang = /\.catala_(fr|en|pl)(\.md)?$/.exec(name)?.[1] as
      | 'fr'
      | 'en'
      | 'pl'
      | undefined;
    if (lang) counts.set(lang, (counts.get(lang) ?? 0) + 1);
  }
  let best: 'fr' | 'en' | 'pl' | undefined;
  for (const [lang, n] of counts) {
    if (best === undefined || n > (counts.get(best) ?? 0)) best = lang;
  }
  return best;
}
