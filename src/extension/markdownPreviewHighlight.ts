// VS Code's built-in Markdown preview highlights fenced code via highlight.js, which
// has no notion of Catala and no plugin point to teach it one. This hooks the preview's
// markdown-it pipeline directly (via contributes["markdown.markdownItPlugins"] +
// extendMarkdownIt, see https://code.visualstudio.com/api/extension-guides/markdown-extension)
// to render ```catala/```catala-metadata fences with a small regex-based approximation
// instead of leaving them unhighlighted.
//
// The word lists below are transcribed from syntaxes/en.xml's and syntaxes/fr.xml's
// `#code` repository (keyword.other/control and support.type buckets) and merged: a
// fence's info string is just "catala"/"catala-metadata" regardless of whether it came
// from a catala_en or catala_fr document, so there's no way to tell which locale's
// keywords to expect.

const KEYWORDS = [
  // English — declarations
  'scope',
  'depends\\s+on',
  'result\\s+of',
  'declaration',
  'includes',
  'list\\s+of',
  'content\\s+of',
  'content',
  'type',
  'optional\\s+of',
  'structure',
  'enumeration',
  'context',
  'input',
  'output',
  'internal',
  'rule',
  'under\\s+condition',
  'condition',
  'data',
  'consequence',
  'fulfilled',
  'equals',
  'assertion',
  'definition',
  'state',
  'label',
  'exception',
  'anything(?:\\s+of)?',
  'list\\s+empty',
  'is\\s+maximum',
  'is\\s+minimum',
  'minimum\\s+of',
  'maximum\\s+of',
  'combine',
  'map\\s+each',
  'to',
  'initially',
  'sort(?:\\s+all)?',
  'in\\s+(?:in|de)creasing\\s+order',
  'and\\s+then',
  'impossible',
  // English — control
  'match',
  'with\\s+pattern',
  'but\\s+replace',
  'fixed',
  'by',
  'down',
  'up',
  'varies',
  'with',
  'we\\s+have',
  'let',
  'in',
  'such\\s+that',
  'exists',
  'contains',
  'among',
  'for',
  'all',
  'of',
  'if',
  'then',
  'else',
  'initial',
  // French — declarations
  "champ\\s+d'application",
  'dépend\\s+de',
  'résultat\\s+de',
  'déclaration',
  'inclus',
  'liste\\s+de',
  'contenu\\s+de',
  'contenu',
  'optionnel\\s+de',
  'énumération',
  'contexte',
  'entrée',
  'résultat',
  'interne',
  'règle',
  'sous\\s+condition',
  'donnée',
  'conséquence',
  'rempli',
  'égal\\s+à',
  'définition',
  'état',
  'étiquette',
  "n'importe\\s+quel(?:\\s+de)?",
  'liste\\s+vide',
  'est\\s+maximum',
  'est\\s+minimum',
  'minimum\\s+de',
  'maximum\\s+de',
  'combine\\s+tout',
  'transforme\\s+chaque',
  'en',
  'initialement',
  'trie(?:\\s+tout)?',
  'par\\s+ordre\\s+(?:dé)?croissant',
  'puis',
  // French — control
  'selon',
  'sous\\s+forme',
  'mais\\s+en\\s+remplaçant',
  'fixé',
  'par',
  'inférieur',
  'supérieur',
  'varie',
  'avec',
  'on\\s+a',
  'soit',
  'dans',
  'tel\\s+que',
  'existe',
  'contient',
  'pour',
  'parmi',
  'tout',
  'de',
  'si',
  'alors',
  'sinon',
].join('|');

const TYPES = [
  'integer',
  'boolean',
  'date',
  'duration',
  'money',
  'code_location',
  'decimal',
  'number',
  'sum',
  'entier',
  'booléen',
  'durée',
  'argent',
  'position_source',
  'décimal',
  'décret',
  'loi',
  'nombre',
  'somme',
].join('|');

// Superset of en.xml's and fr.xml's identifier character classes (fr.xml additionally
// allows ô/Ô).
const UPPER =
  'A-Z\\u00c9\\u00c8\\u00c0\\u00c2\\u00d9\\u00ce\\u00d4\\u00ca\\u0152\\u00c7';
const LOWER =
  'a-z\\u00e9\\u00e8\\u00e0\\u00e2\\u00f9\\u00ee\\u00f4\\u00ea\\u0153\\u00e7';

// JS's \b is ASCII-only ([A-Za-z0-9_]), so it silently fails to match at either edge of
// any French keyword that starts or ends on an accented letter (e.g. "état", "durée").
// \p{L}/\p{N} property escapes (which need the `u` flag) are Unicode-aware instead.
const WORD_START = '(?<![\\p{L}\\p{N}_])';
const WORD_END = '(?![\\p{L}\\p{N}_])';

const TOKEN_RE = new RegExp(
  '(?<comment>#[^\\n]*)' +
    `|(?<keyword>${WORD_START}(?:${KEYWORDS})${WORD_END})` +
    `|(?<type>${WORD_START}(?:${TYPES})${WORD_END})` +
    `|(?<number>\\|[0-9]+-[0-9]+-[0-9]+\\||${WORD_START}(?:true|false|vrai|faux)${WORD_END}|${WORD_START}[0-9]+(?:,[0-9]*)?${WORD_END})` +
    `|(?<klass>${WORD_START}[${UPPER}][${LOWER}${UPPER}0-9_']*${WORD_END})`,
  'gu'
);

const CLASS_BY_GROUP: Record<string, string> = {
  comment: 'cat-comment',
  keyword: 'cat-keyword',
  type: 'cat-type',
  number: 'cat-number',
  klass: 'cat-class',
};

function escapeHtml(s: string): string {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function highlightCatala(code: string): string {
  let out = '';
  let last = 0;
  TOKEN_RE.lastIndex = 0;
  let m: RegExpExecArray | null;
  while ((m = TOKEN_RE.exec(code))) {
    out += escapeHtml(code.slice(last, m.index));
    const group = Object.entries(m.groups ?? {}).find(
      ([, v]) => v !== undefined
    );
    const cls = group ? CLASS_BY_GROUP[group[0]] : undefined;
    out += cls
      ? `<span class="${cls}">${escapeHtml(m[0])}</span>`
      : escapeHtml(m[0]);
    last = m.index + m[0].length;
  }
  out += escapeHtml(code.slice(last));
  return out;
}

// Minimal structural type for what extendMarkdownIt needs to touch — avoids depending
// on `markdown-it`'s own types, which this extension doesn't otherwise depend on (it's
// only ever a transitive dependency pulled in by whichever tool provides the real
// MarkdownIt instance at runtime).
type MarkdownItLike = {
  options: {
    highlight?:
      | ((code: string, lang: string, attrs: string) => string)
      | null
      | undefined;
  };
};

export function extendMarkdownIt<T extends MarkdownItLike>(md: T): T {
  const defaultHighlight = md.options.highlight;
  md.options.highlight = (code, lang, attrs): string =>
    lang === 'catala' || lang === 'catala-metadata'
      ? highlightCatala(code)
      : (defaultHighlight?.(code, lang, attrs) ?? '');
  return md;
}
