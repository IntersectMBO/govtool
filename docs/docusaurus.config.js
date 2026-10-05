// @ts-check
const { themes: prismThemes } = require("prism-react-renderer");

// Build-time settings. Override them with environment variables (or Docker
// build args, see Dockerfile) to build the site for another domain, e.g. a
// temporary host before docs.gov.tools is switched over.
const siteUrl = process.env.DOCS_URL || "https://docs.gov.tools";
const baseUrl = process.env.DOCS_BASE_URL || "/";
// Set DOCS_NO_INDEX=true on temporary hosts (e.g. a github.io preview) so
// search engines do not index a copy of docs.gov.tools.
const noIndex = process.env.DOCS_NO_INDEX === "true";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Governance Tools Documentation",
  tagline: "User guides for Cardano GovTool and the Constitutional Committee Portal",
  favicon: "img/favicon.svg",

  url: siteUrl,
  baseUrl,
  noIndex,

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
          editUrl: "https://github.com/IntersectMBO/govtool/edit/develop/docs/",
          remarkPlugins: [[require("./src/remark/base-url-raw-html"), { baseUrl }]],
        },
        blog: false,
        pages: false,
        theme: {
          customCss: [
            require.resolve("@fontsource-variable/inter/index.css"),
            require.resolve("./src/css/custom.css"),
          ],
        },
      }),
    ],
  ],

  plugins: [
    // The webpack cache in node_modules/.cache is only evicted when this file
    // changes, not when DOCS_URL or DOCS_BASE_URL do. Without this, a build for
    // /govtool/ right after one for / reuses CSS with root font paths.
    function cacheKeyedOnBaseUrl() {
      return {
        name: "cache-keyed-on-base-url",
        configureWebpack(webpackConfig) {
          const { cache } = webpackConfig;
          if (!cache || typeof cache !== "object") return {};
          return { cache: { ...cache, version: `${cache.version}-${siteUrl}${baseUrl}` } };
        },
      };
    },
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
            // "What is Cardano GovTool?" is now the home page.
            from: "/overview/what-is-cardano-govtool",
            to: "/",
          },
          {
            // Linked from the Proposal Discussion submission flow in the GovTool frontend.
            from: "/using-govtool/govtool-functions/storing-information-offline",
            to: "/cardano-govtool/using-govtool/storing-information-offline",
          },
          {
            // The GitBook section root; it has no page of its own here.
            from: "/cardano-govtool",
            to: "/cardano-govtool/using-govtool",
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
      announcementBar: {
        id: "maintenance-2026",
        content:
          `GovTool is actively maintained by the Sireto team. <a href="${baseUrl}important-updates/govtool-maintenance-2026">Read the 2026 update</a>`,
        isCloseable: true,
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
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
      },
    }),
};

module.exports = config;
