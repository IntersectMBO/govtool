import fs from "fs";
import path from "path";
import {
  defineConfig as defineViteConfig,
  mergeConfig,
  type Plugin,
} from "vite";
import { defineConfig as defineVitestConfig } from "vitest/config";
import compression from "vite-plugin-compression";
import react from "@vitejs/plugin-react-swc";

/**
 * `@intersect.mbo/pdf-ui` injects a stylesheet whose Poppins `@font-face` rules
 * point at `./files/poppins-*`, but the package ships no `files` folder. Those
 * requests resolve against the page, reach the SPA fallback, and come back as
 * HTML the browser cannot decode as a font. Serve them from
 * `@fontsource/poppins`, which has the same files, in dev and in the build.
 */
const POPPINS_DIR = path.resolve(
  __dirname,
  "node_modules/@fontsource/poppins/files",
);
const POPPINS_FILE = /^\/files\/(poppins-[a-z0-9-]+\.woff2?)$/;

const pdfUiFonts = (): Plugin => ({
  name: "pdf-ui-fonts",
  configureServer(server) {
    server.middlewares.use((req, res, next) => {
      const match = POPPINS_FILE.exec((req.url ?? "").split("?")[0]);
      const file = match && path.join(POPPINS_DIR, match[1]);
      if (!file || !fs.existsSync(file)) {
        next();
        return;
      }
      res.setHeader(
        "Content-Type",
        file.endsWith(".woff2") ? "font/woff2" : "font/woff",
      );
      res.setHeader("Cache-Control", "public, max-age=31536000, immutable");
      fs.createReadStream(file).pipe(res);
    });
  },
  generateBundle() {
    if (!fs.existsSync(POPPINS_DIR)) return;
    for (const name of fs.readdirSync(POPPINS_DIR)) {
      if (!/^poppins-latin-[0-9]+-(normal|italic)\.woff2?$/.test(name))
        continue;
      this.emitFile({
        type: "asset",
        fileName: `files/${name}`,
        source: fs.readFileSync(path.join(POPPINS_DIR, name)),
      });
    }
  },
});

const viteConfig = defineViteConfig({
  plugins: [
    react(),
    pdfUiFonts(),
    compression({
      algorithm: "brotliCompress",
      threshold: 1024 * 10,
    }),
  ],
  cacheDir: ".vite",
  define: {
    "process.env": {},
  },
  build: {
    chunkSizeWarningLimit: 512,
    minify: "terser",
    terserOptions: {
      compress: {
        keep_infinity: true,
        drop_console: true,
        drop_debugger: true,
      },
    },
  },
  resolve: {
    alias: [
      { find: "@", replacement: path.resolve(__dirname, "./src") },
      { find: "@pages", replacement: path.resolve(__dirname, "./src/pages") },
      { find: "@consts", replacement: path.resolve(__dirname, "./src/consts") },
      { find: "@mock", replacement: path.resolve(__dirname, "./src/mock") },
      {
        find: "@services",
        replacement: path.resolve(__dirname, "./src/services"),
      },
      { find: "@hooks", replacement: path.resolve(__dirname, "./src/hooks") },
      {
        find: "@atoms",
        replacement: path.resolve(__dirname, "./src/components/atoms"),
      },
      {
        find: "@molecules",
        replacement: path.resolve(__dirname, "./src/components/molecules"),
      },
      {
        find: "@organisms",
        replacement: path.resolve(__dirname, "./src/components/organisms"),
      },
      {
        find: "@context",
        replacement: path.resolve(__dirname, "./src/context"),
      },
      {
        find: "@models",
        replacement: path.resolve(__dirname, "./src/models"),
      },
      {
        find: "@utils",
        replacement: path.resolve(__dirname, "./src/utils"),
      },
    ],
  },
});

const vitestConfig = defineVitestConfig({
  test: {
    setupFiles: "./src/setupTests.ts",
    testTimeout: 10000,
    globals: true,
    pool: "forks",
    poolOptions: {
      threads: {
        minThreads: 2,
      },
    },
    maxConcurrency: 4,
    environment: "jsdom",
    reporters: ["default", "junit"],
    outputFile: {
      junit: "./junit-report.xml",
      json: "./json-report.json",
    },
    coverage: {
      include: [
        "src/components/**/*",
        "src/consts/**/*",
        "src/context/**/*",
        "src/hooks/**/*",
        "src/services/**/*",
        "src/utils/**/*",
      ],
      provider: "v8",
      reporter: ["json-summary", "lcov"],
      reportOnFailure: true,
      enabled: true,
    },
  },
});

export default mergeConfig(viteConfig, vitestConfig);
