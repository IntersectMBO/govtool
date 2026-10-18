#!/usr/bin/env python3
# Usage: python3 gitbook-to-docusaurus.py <governance-tools-documentation>/gitbook <govtool repo>/docs
"""Convert the GitBook export in governance-tools-documentation to Docusaurus docs."""
import json, os, posixpath, re, shutil, sys
from urllib.parse import quote, unquote

SRC = sys.argv[1]          # .../gtdocs/gitbook
OUT = sys.argv[2]          # <govtool repo>/docs (Docusaurus site root)
DOCS = os.path.join(OUT, "docs")
IMG_DIR = os.path.join(OUT, "static", "img", "gitbook")
IMG_URL = "/img/gitbook/"

RENAMES = {
    "README (1).md": "overview/core-governance-tools/README.md",
    "cardano-govtool/faqs/how-was-the-author-of-withdraw-ara45-217-for-mlabs-core...-ga-verified.md":
        "cardano-govtool/faqs/how-was-the-author-of-withdraw-ara45-217-for-mlabs-core-ga-verified.md",
}
HINT = {"info": "info", "warning": "warning", "danger": "danger", "success": "tip"}

all_md = []
for root, dirs, files in os.walk(SRC):
    dirs[:] = [d for d in dirs if d != ".gitbook"]
    for f in files:
        if f.endswith(".md") and f != "SUMMARY.md":
            all_md.append(posixpath.relpath(os.path.join(root, f), SRC).replace(os.sep, "/"))
all_set = set(all_md)
new_path = {p: RENAMES.get(p, p) for p in all_md}
used_assets, missing_assets, broken_links = set(), set(), []

def resolve_md(src_rel, target):
    """Map a link target in src_rel (old path) to a new-path relative link, or None."""
    t = unquote(target)
    path, _, anchor = t.partition("#")
    if not path:
        return None
    abs_old = posixpath.normpath(posixpath.join(posixpath.dirname(src_rel), path))
    cand = None
    if abs_old in all_set:
        cand = abs_old
    elif posixpath.join(abs_old, "README.md") in all_set:
        cand = posixpath.join(abs_old, "README.md")
    elif abs_old + ".md" in all_set:
        cand = abs_old + ".md"
    if cand is None:
        return None
    return cand, anchor

def asset_url(target):
    name = posixpath.basename(unquote(target.strip()))
    src = os.path.join(SRC, ".gitbook", "assets", name)
    if os.path.exists(src):
        used_assets.add(name)
    else:
        missing_assets.add(name)
    return IMG_URL + quote(name)

def rewrite_target(src_rel, cur_new, target):
    t = target.strip()
    if ".gitbook/assets/" in t:
        return asset_url(t)
    if re.match(r"^[a-z][a-z0-9+.-]*:", t, re.I) or t.startswith("#") or t.startswith("/"):
        return target
    r = resolve_md(src_rel, t)
    if r is None:
        broken_links.append((src_rel, t))
        return target
    cand, anchor = r
    rel = posixpath.relpath(new_path[cand], posixpath.dirname(cur_new) or ".")
    if not rel.startswith("."):
        rel = "./" + rel
    return quote(rel, safe="/.-_~()") + ("#" + anchor if anchor else "")

LINK_RE = re.compile(r'(\]\()(<[^>]*>|[^)\s]+)((?:\s+"[^"]*")?\))')
SRC_RE = re.compile(r'((?:src|href)=")([^"]+)(")')

def rewrite_links(text, src_rel, cur_new):
    def md(m):
        tgt = m.group(2)
        angled = tgt.startswith("<")
        inner = tgt[1:-1] if angled else tgt
        new = rewrite_target(src_rel, cur_new, inner)
        return m.group(1) + new + m.group(3)
    text = LINK_RE.sub(md, text)
    text = SRC_RE.sub(lambda m: m.group(1) + rewrite_target(src_rel, cur_new, m.group(2)).replace("&#x26;", "&") + m.group(3), text)
    return text

def embed(url):
    m = re.match(r"https://www\.loom\.com/share/([0-9a-f]+)", url)
    if m:
        return ('<div class="gitbook-embed"><iframe src="https://www.loom.com/embed/%s" '
                'title="Loom video" allowfullscreen loading="lazy"></iframe></div>\n\n[%s](%s)' % (m.group(1), url, url))
    m = re.match(r"https://drive\.google\.com/file/d/([^/]+)/", url)
    if m:
        return ('<div class="gitbook-embed"><iframe src="https://drive.google.com/file/d/%s/preview" '
                'title="Google Drive file" allowfullscreen loading="lazy"></iframe></div>\n\n[%s](%s)' % (m.group(1), url, url))
    return "[%s](%s)" % (url, url)

def split_fm(text):
    if text.startswith("---\n"):
        end = text.find("\n---\n", 4)
        if end != -1:
            return text[4:end], text[end + 5:]
    return None, text

def convert(src_rel, text, cur_new):
    fm, body = split_fm(text)
    cover = None
    keep = []
    if fm:
        lines = fm.split("\n")
        i = 0
        while i < len(lines):
            line = lines[i]
            key = line.split(":", 1)[0]
            block = [line]
            i += 1
            while i < len(lines) and (lines[i].startswith(" ") or lines[i] == ""):
                block.append(lines[i]); i += 1
            if key == "cover":
                cover = line.split(":", 1)[1].strip()
            elif key in ("coverY", "layout", "icon"):
                continue
            else:
                keep.extend(block)
    if src_rel == "README.md":
        keep.append("slug: /")

    def inc(m):
        inc_rel = posixpath.normpath(posixpath.join(posixpath.dirname(src_rel), m.group(1)))
        _, ib = split_fm(open(os.path.join(SRC, inc_rel), encoding="utf-8").read())
        # links inside the include are relative to the include file
        return rewrite_links(ib.strip(), inc_rel, cur_new)
    body = re.sub(r'\{%\s*include\s+"([^"]+)"\s*%\}', inc, body)
    body = re.sub(r'\{%\s*hint\s+style="(\w+)"\s*%\}', lambda m: ":::" + HINT.get(m.group(1), "note"), body)
    body = re.sub(r'\{%\s*endhint\s*%\}', ":::", body)
    body = re.sub(r'\{%\s*embed\s+url="([^"]+)"\s*%\}', lambda m: embed(m.group(1)), body)
    body = rewrite_links(body, src_rel, cur_new)
    if cover:
        url = asset_url(cover)
        body = re.sub(r"^(# .*\n)", lambda m: m.group(1) + '\n<img src="%s" alt="" class="gitbook-cover" />\n' % url, body, count=1, flags=re.M)
    left = re.findall(r"\{%[^%]*%\}", body)
    if left:
        print("WARN unconverted gitbook tags in", src_rel, left)
    out = ("---\n" + "\n".join(keep).strip("\n") + "\n---\n" if keep else "") + body
    return out

# Only the GitBook-owned paths are regenerated; hand-written pages (e.g. docs/developers) are left alone.
for top in sorted({np_.split("/")[0] for np_ in new_path.values()}):
    target = os.path.join(DOCS, top)
    if os.path.isdir(target):
        shutil.rmtree(target)
    elif os.path.isfile(target):
        os.remove(target)
if os.path.exists(IMG_DIR):
    shutil.rmtree(IMG_DIR)
os.makedirs(IMG_DIR)
for p in all_md:
    np_ = new_path[p]
    dst = os.path.join(DOCS, np_)
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    with open(os.path.join(SRC, p), encoding="utf-8") as fh:
        txt = fh.read()
    with open(dst, "w", encoding="utf-8") as fh:
        fh.write(convert(p, txt, np_))
for name in sorted(used_assets):
    shutil.copy2(os.path.join(SRC, ".gitbook", "assets", name), os.path.join(IMG_DIR, name))

# ---- sidebar from SUMMARY.md ----
def doc_id(rel):
    return posixpath.splitext(new_path[rel])[0]

tree, stack = [], []   # nodes: {"label","target","children"} or {"section"}
for line in open(os.path.join(SRC, "SUMMARY.md"), encoding="utf-8"):
    line = line.rstrip("\n")
    h = re.match(r"^## (.+)$", line)
    if h:
        tree.append({"section": h.group(1).strip()})
        stack = []
        continue
    m = re.match(r"^(\s*)\* \[(.*)\]\((<[^>]*>|[^)]*)\)", line)
    if not m:
        continue
    indent = len(m.group(1))
    node = {"label": m.group(2), "target": m.group(3).strip("<>"), "children": []}
    while stack and stack[-1][0] >= indent:
        stack.pop()
    (stack[-1][1]["children"] if stack else tree).append(node)
    stack.append((indent, node))

def emit(node):
    if "section" in node:
        return {"type": "html", "value": node["section"], "className": "sidebar-section", "defaultStyle": True}
    if re.match(r"^https?://", node["target"]):
        return {"type": "link", "label": node["label"], "href": node["target"]}
    did = doc_id(node["target"])
    if node["children"]:
        return {"type": "category", "label": node["label"], "link": {"type": "doc", "id": did},
                "collapsed": True, "items": [emit(c) for c in node["children"]]}
    return {"type": "doc", "id": did, "label": node["label"]}

with open(os.path.join(OUT, "sidebars.gitbook.js"), "w", encoding="utf-8") as fh:
    fh.write("// Generated by scripts/gitbook-to-docusaurus.py from the GitBook SUMMARY.md of\n")
    fh.write("// IntersectMBO/governance-tools-documentation. Do not edit by hand; edit sidebars.js instead.\n")
    fh.write("module.exports = " + json.dumps([emit(n) for n in tree], indent=2, ensure_ascii=False) + ";\n")

print("docs:", len(all_md), "assets copied:", len(used_assets))
if missing_assets:
    print("MISSING assets:", sorted(missing_assets))
for b in broken_links:
    print("UNRESOLVED link:", b)
