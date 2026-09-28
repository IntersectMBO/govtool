// @ts-check
const { themes: prismThemes } = require("prism-react-renderer");

// Build-time settings. Override them with environment variables (or Docker
// build args, see Dockerfile) to build the site for another domain, e.g. a
// temporary host before docs.gov.tools is switched over.
const siteUrl = process.env.DOCS_URL || "https://docs.gov.tools";
const baseUrl = process.env.DOCS_BASE_URL || "/";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Governance Tools Documentation",
  tagline: "User guides for Cardano GovTool and the Constitutional Committee Portal",
  favicon: "img/favicon.svg",

  url: siteUrl,
  baseUrl,

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

  plugins: [
    [
      "@docusaurus/plugin-client-redirects",
      {
        // Old URLs that are still linked from GovTool, the CC portal or GitBook.
        redirects: [
          {
            from: "/cardano-govtool/faqs/how-was-the-author-of-withdraw-ara45-217-for-mlabs-core...-ga-verified",
            to: "/cardano-govtool/faqs/how-was-the-author-of-withdraw-ara45-217-for-mlabs-core-ga-verified",
          },
          {
            from: "/about/what-is-the-constitutional-committee-portal",
            to: "/overview/what-is-the-constitutional-committee-portal",
          },
        ],
      },
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
              { label: "Privacy Policy", to: "/legal/privacy-policy" },
              { label: "Terms of Use", to: "/legal/terms-of-use" },
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
