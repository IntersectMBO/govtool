// @ts-check
const gitbookSidebar = require("./sidebars.gitbook.js");

// The GitBook "Legal" section linked to Intersect's policies; it is replaced
// below by GovTool's own Privacy Policy and Terms of Use pages.
const legalIndex = gitbookSidebar.findIndex(
  (item) => item.type === "html" && item.value === "Legal",
);
const userDocs = (legalIndex === -1 ? gitbookSidebar : gitbookSidebar.slice(0, legalIndex)).slice();

// Add the Support page right after the GovTool FAQs.
const faqsIndex = userDocs.findIndex(
  (item) => item.type === "category" && item.link && item.link.id === "cardano-govtool/faqs/README",
);
if (faqsIndex === -1) {
  throw new Error("sidebars.js: GovTool FAQs category not found; cannot place the Support page.");
}
userDocs.splice(faqsIndex + 1, 0, { type: "doc", id: "cardano-govtool/support", label: "Support" });

const legal = [
  { type: "html", value: "Legal", className: "sidebar-section", defaultStyle: true },
  { type: "doc", id: "legal/privacy-policy", label: "Privacy Policy" },
  { type: "doc", id: "legal/terms-of-use", label: "Terms of Use" },
];

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  docsSidebar: [
    ...userDocs,
    {
      type: "html",
      value: "Developer documentation",
      className: "sidebar-section",
      defaultStyle: true,
    },
    { type: "doc", id: "developers/architecture/README", label: "Software Architecture" },
    {
      type: "doc",
      id: "developers/governance-action-submission",
      label: "Submit a Governance Action",
    },
    {
      type: "doc",
      id: "developers/operations/handle-new-governance-action-type",
      label: "Handle a New Governance Action Type",
    },
    {
      type: "category",
      label: "Style Guides",
      collapsed: true,
      items: [
        { type: "doc", id: "developers/style-guides/react/README", label: "React/JSX" },
        { type: "doc", id: "developers/style-guides/css-in-js/README", label: "CSS-in-JavaScript" },
        { type: "doc", id: "developers/style-guides/css-sass/README", label: "CSS / Sass" },
      ],
    },
    ...legal,
  ],
};

module.exports = sidebars;
