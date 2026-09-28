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

## Deployment (Docker)

The site is packaged as a static nginx image (`Dockerfile`, `nginx.conf.template`). The container runs as a non-root user and listens on port `8080`, with a health check at `/healthz`.

### Build and publish the image

Build for `linux/amd64` (servers) even when building on an Apple Silicon Mac:

```sh
cd docs
docker buildx build --platform linux/amd64 \
  --build-arg DOCS_URL=https://docs.dev.gov.tools \
  -t ghcr.io/<owner>/govtool-docs:<tag> \
  --push .
```

Build arguments (fixed at build time):

| Argument | Default | Purpose |
| --- | --- | --- |
| `DOCS_URL` | `https://docs.gov.tools` | Public URL of the site (canonical links, Open Graph tags, sitemap) |
| `DOCS_BASE_URL` | `/` | Path the site is served under |

Runtime environment variables:

| Variable | Default | Purpose |
| --- | --- | --- |
| `X_ROBOTS_TAG` | `all` | Value of the `X-Robots-Tag` header. Use `noindex, nofollow` on temporary hosts |

CI (`.github/workflows/build-docker-images.yml`) also builds and pushes `ghcr.io/intersectmbo/govtool-docs` on `main`, `develop`, `test` and version tags. It uses the `DOCS_URL` repository variable when set.

### Run on a server

`deploy/` contains a Compose file, an `.env.example` and an example host nginx server block:

```sh
cd deploy
cp .env.example .env        # set DOCS_IMAGE, DOCS_PORT, X_ROBOTS_TAG
docker compose up -d
curl -fsS http://127.0.0.1:8085/healthz
```

The container is published on `127.0.0.1` only. The host reverse proxy terminates TLS and forwards to it (see `deploy/nginx-host.conf.example`).

### Moving to docs.gov.tools

1. Rebuild the image with `DOCS_URL=https://docs.gov.tools` (the default).
2. Set `X_ROBOTS_TAG=all` and redeploy.
3. Point the `docs.gov.tools` DNS record at the server and add it to the host reverse proxy.
4. Update the GovTool frontend footer to link to `/legal/privacy-policy` and `/legal/terms-of-use` on docs.gov.tools.

## Layout

| Path | Purpose |
| --- | --- |
| `docs/` | Markdown pages (URL = file path, `README.md` = section index) |
| `docs/developers/` | Developer documentation (hand-written, not touched by the migration script) |
| `Dockerfile`, `nginx.conf.template`, `deploy/` | Docker image and server deployment files |
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
