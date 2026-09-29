import fs from 'node:fs';
import path from 'node:path';
// GovTool's theme and consts barrel import each other; loading the barrel
// first gives the same evaluation order as the app.
import '@/consts';
import { theme as govtoolTheme } from '@/theme';
import pdfTheme from './theme';

const PDF_UI_ROOT = path.resolve(process.cwd(), 'src/pdf-ui');
const PALETTE_ROOTS =
    'primary|secondary|text|border|badgeColors|iconButton|highlight|divider|background|action|error|success|warning|info';

const listSources = (dir) =>
    fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
        const full = path.join(dir, entry.name);
        if (entry.isDirectory()) return listSources(full);
        return /\.(jsx?|tsx?)$/.test(entry.name) && !/\.test\./.test(entry.name)
            ? [full]
            : [];
    });

// Every `palette.a.b` member chain (joined across lines and optional
// chaining) and every `'a.b'` sx string token with a palette root.
const collectPalettePaths = () => {
    const paths = new Set();
    listSources(PDF_UI_ROOT).forEach((file) => {
        const source = fs.readFileSync(file, 'utf8');
        const joined = source.replace(/\s*(\?\.|\.)\s*(?=[A-Za-z_])/g, '.');
        [...joined.matchAll(/palette\.([A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*)/g)].forEach(
            (m) => paths.add(m[1])
        );
        const tokenRe = new RegExp(
            `['"\`]((?:${PALETTE_ROOTS})\\.[A-Za-z_]\\w*(?:\\.[A-Za-z_]\\w*)*)['"\`]`,
            'g'
        );
        [...source.matchAll(tokenRe)].forEach((m) => paths.add(m[1]));
    });
    // Module names such as 'highlight.js' match the token pattern too.
    return [...paths].filter((p) => !/\.(js|css)$/.test(p)).sort();
};

const read = (obj, dotted) =>
    dotted.split('.').reduce((acc, key) => (acc == null ? acc : acc[key]), obj);

// The old pdf-ui palette keys. They were compat aliases during the restyle and
// must not come back: GovTool's palette does not define them.
const OLD_KEYS = [
    /^primary\.lightGray$/,
    /^primary\.icons(\.|$)/,
    /^divider\.primary$/,
    /^badgeColors(\.|$)/,
    /^iconButton(\.|$)/,
    /^text\.(grey|darkPurple|black|orange)$/,
    /^border(\.|$)/,
    /^highlight(\.|$)/,
];

describe('pdf-ui theme', () => {
    const palettePaths = collectPalettePaths();

    it('finds the palette reads in pdf-ui', () => {
        expect(palettePaths.length).toBeGreaterThan(3);
    });

    it('no longer reads the old pdf-ui palette keys', () => {
        expect(
            palettePaths.filter((p) => OLD_KEYS.some((re) => re.test(p)))
        ).toEqual([]);
    });

    it('defines none of the old pdf-ui palette keys', () => {
        [
            'badgeColors',
            'iconButton',
            'border',
            'highlight',
        ].forEach((key) => expect(pdfTheme.palette[key]).toBeUndefined());
        expect(pdfTheme.palette.primary.lightGray).toBeUndefined();
        expect(pdfTheme.palette.primary.icons).toBeUndefined();
        expect(pdfTheme.palette.text.grey).toBeUndefined();
        expect(pdfTheme.palette.divider).toBe(govtoolTheme.palette.divider);
    });

    it.each(palettePaths)('palette.%s resolves to a colour', (dotted) => {
        const value = read(pdfTheme.palette, dotted);
        expect(typeof value).toBe('string');
        expect(value.length).toBeGreaterThan(0);
    });

    it('uses the GovTool breakpoints', () => {
        expect(pdfTheme.breakpoints.values).toEqual(
            govtoolTheme.breakpoints.values
        );
        expect(pdfTheme.breakpoints.keys).toEqual(govtoolTheme.breakpoints.keys);
        expect(pdfTheme.breakpoints.up('md')).toBe('@media (min-width:768px)');
        expect(pdfTheme.breakpoints.down('md')).toBe(
            '@media (max-width:767.95px)'
        );
    });

    it('keeps the GovTool MuiInputBase pill override', () => {
        expect(pdfTheme.components.MuiInputBase).toEqual(
            govtoolTheme.components.MuiInputBase
        );
    });

    it('scopes the remaining TextField restyle to TextField', () => {
        const root = pdfTheme.components.MuiTextField.styleOverrides.root;
        expect(root['& .MuiInputBase-root']).toMatchObject({ padding: 0 });
        expect(
            root['& .MuiOutlinedInput-notchedOutline'].borderColor
        ).toBe(govtoolTheme.palette.secondaryBlue);
        expect(govtoolTheme.components.MuiTextField).toBeUndefined();
    });

    it('no longer neutralises MuiFormHelperText', () => {
        expect(pdfTheme.components.MuiFormHelperText).toBeUndefined();
    });

    it('builds on the GovTool theme', () => {
        expect(pdfTheme.typography.fontFamily).toBe('Poppins, Arial');
        expect(pdfTheme.palette.primary.main).toBe('#0033AD');
        expect(pdfTheme.palette.lightBlue).toBe('#D6E2FF');
        expect(pdfTheme.shadows[1]).toBe(govtoolTheme.shadows[1]);
        expect(pdfTheme.components.MuiChip).toBeDefined();
        expect(pdfTheme.components.MuiCard).toBeDefined();
        expect(pdfTheme.components.MuiButton).toEqual(
            govtoolTheme.components.MuiButton
        );
    });

    it('leaves the GovTool theme untouched', () => {
        expect(govtoolTheme.breakpoints.values.md).toBe(768);
        expect(govtoolTheme.palette.badgeColors).toBeUndefined();
    });
});
