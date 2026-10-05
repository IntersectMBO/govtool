// Checks the asset paths in a built site. Docusaurus checks Markdown images and
// links itself, but not the raw HTML <img> tags left by the GitBook export, nor
// url(...) references in the CSS. This fails when:
//
// - a page or stylesheet references an image, font or other file under
//   <baseUrl>img/ or <baseUrl>assets/ that is not in the build, or
// - with a base path other than /, a page or stylesheet uses a root-relative
//   path ("/...", not "//host/...") without that base path, which would 404
//   where the site is served under the base path (e.g. the GitHub Pages
//   preview at /govtool/).
//
// Usage: node scripts/check-images.mjs [buildDir] (run after npm run build,
// with the same DOCS_BASE_URL)
import { readdirSync, readFileSync, existsSync } from "node:fs";
import { join } from "node:path";

const buildDir = process.argv[2] || "build";
const baseUrl = process.env.DOCS_BASE_URL || "/";

// src="...", href="..." and url(...) values that start with a single slash.
const htmlRef = /\b(?:src|href)="(\/(?!\/)[^"]*)"/g;
const cssRef = /url\(\s*["']?(\/(?!\/)[^"')]*)["']?\s*\)/g;

function* files(dir) {
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) yield* files(path);
    else if (entry.name.endsWith(".html") || entry.name.endsWith(".css")) yield path;
  }
}

const problems = new Map();
let checked = 0;
for (const file of files(buildDir)) {
  const text = readFileSync(file, "utf8");
  const pattern = file.endsWith(".css") ? cssRef : htmlRef;
  for (const [, ref] of text.matchAll(pattern)) {
    checked++;
    const path = ref.split(/[?#]/)[0];
    if (!path.startsWith(baseUrl)) {
      problems.set(ref, `missing the base path ${baseUrl} (first used in ${file})`);
      continue;
    }
    const relative = path.slice(baseUrl.length);
    if (!relative.startsWith("img/") && !relative.startsWith("assets/")) continue;
    if (!existsSync(join(buildDir, decodeURIComponent(relative)))) {
      problems.set(ref, `not in the build (first used in ${file})`);
    }
  }
}

if (problems.size > 0) {
  for (const [ref, why] of problems) console.error(`${ref}: ${why}`);
  process.exit(1);
}
console.log(`${checked} root-relative references checked under ${baseUrl}, no problems`);
