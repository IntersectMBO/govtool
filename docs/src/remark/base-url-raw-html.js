// Prefixes root-relative src/href attributes in raw HTML with the site's
// baseUrl. Docusaurus rewrites Markdown links and images itself, but not the
// <img src="/img/..."> tags the GitBook export left in the pages, so those
// break when the site is served under a sub-path (e.g. a GitHub Pages
// project site at /govtool/).
function walk(node, fn) {
  fn(node);
  if (node.children) node.children.forEach((child) => walk(child, fn));
}

module.exports = function baseUrlRawHtml({ baseUrl = "/" } = {}) {
  if (baseUrl === "/") return () => {};
  return (tree) => {
    walk(tree, (node) => {
      if (node.type !== "html") return;
      // Leave protocol-relative URLs (//host/...) and already-prefixed paths alone.
      node.value = node.value.replace(/\b(src|href)="\/(?!\/)/g, (match, attr, offset, value) =>
        value.startsWith(baseUrl, offset + attr.length + 2) ? match : `${attr}="${baseUrl}`,
      );
    });
  };
};
