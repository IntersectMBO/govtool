# GovTool Documentation

Source for [docs.gov.tools](https://docs.gov.tools), built with [Docusaurus](https://docusaurus.io/).

It contains:

- **User guides**, migrated as-is from the GitBook export in
  [IntersectMBO/governance-tools-documentation](https://github.com/IntersectMBO/governance-tools-documentation)
  (`gitbook/` directory). Page paths, sidebar order and labels match the original GitBook `SUMMARY.md`.
- **Legal pages** (`docs/legal/`): GovTool's Privacy Policy and Terms of Use.
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
| `sidebars.gitbook.js` | GitBook part of the sidebar (originally generated from `SUMMARY.md`) |
| `static/img/gitbook/` | Images referenced by the pages |
| `src/css/custom.css` | Theme overrides and styles for GitBook-exported HTML (figures, embeds) |
| `scripts/gitbook-to-docusaurus.py` | One-off converter used for the migration (reference only) |

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

## Migration script

`scripts/gitbook-to-docusaurus.py` was used once to convert the GitBook export. The pages have been edited since then to match the current GovTool implementation, so **do not re-run it** on this folder: it would overwrite those edits. It is kept for reference only.
