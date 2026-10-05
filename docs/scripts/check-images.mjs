// Fails when a built page references an image under <baseUrl>img/ that is not
// in the build. Docusaurus checks Markdown images and links itself, but not
// the raw HTML <img> tags left by the GitBook export.
//
// Usage: node scripts/check-images.mjs [buildDir] (run after npm run build)
import { readdirSync, readFileSync, existsSync } from "node:fs";
import { join } from "node:path";

const buildDir = process.argv[2] || "build";
const baseUrl = process.env.DOCS_BASE_URL || "/";
const pattern = new RegExp(`(?:src|href)="${baseUrl.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}(img/[^"]+)"`, "g");

function* htmlFiles(dir) {
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) yield* htmlFiles(path);
    else if (entry.name.endsWith(".html")) yield path;
  }
}

const missing = new Map();
let checked = 0;
for (const file of htmlFiles(buildDir)) {
  for (const [, ref] of readFileSync(file, "utf8").matchAll(pattern)) {
    checked++;
    const target = join(buildDir, decodeURIComponent(ref.split(/[?#]/)[0]));
    if (!existsSync(target)) missing.set(ref, file);
  }
}

if (missing.size > 0) {
  for (const [ref, file] of missing) console.error(`missing ${ref} (first used in ${file})`);
  process.exit(1);
}
console.log(`${checked} image references checked, none missing`);
