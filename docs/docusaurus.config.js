// @ts-check
const { themes: prismThemes } = require("prism-react-renderer");

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Governance Tools Documentation",
  tagline: "User guides for Cardano GovTool and the Constitutional Committee Portal",
  favicon: "img/favicon.svg",

  url: "https://docs.gov.tools",
  baseUrl: "/",

  organizationName: "IntersectMBO",
  projectName: "govtool",

  onBrokenLinks: "throw",
  onBrokenAnchors: "warn",

  markdown: {
    // Pages were exported from GitBook as CommonMark with inline HTML, so
    // .md files are parsed as plain Markdown rather than MDX.
    format: "detect",
    hooks: {
      onBrokenMarkdownLinks: "throw",
    },
  },

  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          routeBasePath: "/",
          sidebarPath: require.resolve("./sidebars.js"),
          editUrl: "https://github.com/IntersectMBO/govtool/tree/develop/docs/",
        },
        blog: false,
        pages: false,
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      colorMode: {
        respectPrefersColorScheme: true,
      },
      navbar: {
        title: "Governance Tools",
        logo: {
          alt: "GovTool logo",
          src: "img/logo.svg",
        },
        items: [
          { href: "https://gov.tools", label: "GovTool", position: "right" },
          { href: "https://github.com/IntersectMBO/govtool", label: "GitHub", position: "right" },
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Tools",
            items: [
              { label: "Cardano GovTool", href: "https://gov.tools" },
              { label: "Constitutional Committee Portal", href: "https://constitution.gov.tools" },
            ],
          },
          {
            title: "Community",
            items: [
              { label: "GitHub", href: "https://github.com/IntersectMBO/govtool" },
              { label: "Intersect", href: "https://www.intersectmbo.org" },
            ],
          },
          {
            title: "Legal",
            items: [
              { label: "Privacy Policy", href: "https://docs.intersectmbo.org/legal/policies-and-conditions/privacy-policy" },
              { label: "Terms of Use", href: "https://docs.intersectmbo.org/legal/policies-and-conditions/terms-of-use" },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Intersect MBO`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
      },
    }),
};

module.exports = config;
