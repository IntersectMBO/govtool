# GovTool Documentation

Source for [docs.gov.tools](https://docs.gov.tools), built with [Docusaurus](https://docusaurus.io/).

It contains:

- **User guides**, migrated as-is from the GitBook export in
  [IntersectMBO/governance-tools-documentation](https://github.com/IntersectMBO/governance-tools-documentation)
  (`gitbook/` directory). Page paths, sidebar order and labels match the original GitBook `SUMMARY.md`.
- **Developer documentation** (`docs/developers/`): architecture, governance action submission,
  handling new governance action types, and the React / CSS style guides that previously lived in this folder.

## Local development

Requires Node.js >= 22.22.0.

```sh
cd docs
npm ci
npm start        # dev server on http://localhost:3000
npm run build    # static site in ./build
npm run serve    # serve the production build
```

## Layout

| Path | Purpose |
| --- | --- |
| `docs/` | Markdown pages (URL = file path, `README.md` = section index) |
| `docs/developers/` | Developer documentation (hand-written, not touched by the migration script) |
| `sidebars.js` | Sidebar tree: GitBook sections + developer documentation |
| `sidebars.gitbook.js` | GitBook part of the sidebar, generated from `SUMMARY.md` |
| `static/img/gitbook/` | Images referenced by the pages |
| `src/css/custom.css` | Theme overrides and styles for GitBook-exported HTML (figures, embeds) |
| `scripts/gitbook-to-docusaurus.py` | One-off converter used for the migration |

## Writing pages

Pages are plain Markdown (`.md`, parsed as CommonMark with inline HTML). Use `.mdx` if a page needs React components.
Callouts use Docusaurus admonitions:

```md
:::info
Text
:::
```

Supported types: `note`, `tip`, `info`, `warning`, `danger`.

After adding a page, add it to `sidebars.js`.

## Re-running the migration

If the GitBook content changes before the switch-over, regenerate the GitBook pages, `static/img/gitbook/` and `sidebars.gitbook.js` (`docs/developers/` and `sidebars.js` are left untouched):

```sh
git clone --depth 1 https://github.com/IntersectMBO/governance-tools-documentation /tmp/gtdocs
python3 scripts/gitbook-to-docusaurus.py /tmp/gtdocs/gitbook .
```
