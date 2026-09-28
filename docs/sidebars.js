// @ts-check
const gitbookSidebar = require("./sidebars.gitbook.js");

// Legal links stay at the bottom, after the developer documentation.
const legalIndex = gitbookSidebar.findIndex(
  (item) => item.type === "html" && item.value === "Legal",
);
const userDocs = legalIndex === -1 ? gitbookSidebar : gitbookSidebar.slice(0, legalIndex);
const legal = legalIndex === -1 ? [] : gitbookSidebar.slice(legalIndex);

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
