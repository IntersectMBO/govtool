import fs from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";

const ROOT = process.cwd();
const SRC = path.join(ROOT, "src");

const listSources = (dir: string): string[] =>
  fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      return full === path.join(SRC, "pdf-ui") ? [] : listSources(full);
    }
    return /\.(tsx?|jsx?|css)$/.test(entry.name) && !/\.test\./.test(entry.name)
      ? [full]
      : [];
  });

describe("fonts", () => {
  it("does not load fonts from Google", () => {
    const html = fs.readFileSync(path.join(ROOT, "index.html"), "utf8");
    expect(html).not.toContain("fonts.googleapis.com");
  });

  it("has no hard-coded Poppins outside the forum", () => {
    const offenders = listSources(SRC).filter((file) =>
      fs.readFileSync(file, "utf8").includes("Poppins"),
    );
    expect(offenders.map((file) => path.relative(ROOT, file))).toEqual([]);
  });

  it("loads the self-hosted fonts at startup", () => {
    const main = fs.readFileSync(path.join(SRC, "main.tsx"), "utf8");
    expect(main).toContain('import "./fonts"');
  });
});
