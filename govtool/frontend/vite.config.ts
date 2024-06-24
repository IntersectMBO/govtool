import path from "path";
import {
  defineConfig as defineViteConfig,
  mergeConfig,
  transformWithEsbuild,
} from "vite";
import { defineConfig as defineVitestConfig } from "vitest/config";
import react from "@vitejs/plugin-react";

// https://vitejs.dev/config/
const viteConfig = defineViteConfig({
  plugins: [
    {
      name: "jsx-in-js-transformer",
      async transform(code, id) {
        if (!id.match(/.*\.js$/)) return null;

        // Use the exposed transform from vite, instead of directly
        // transforming with esbuild
        return transformWithEsbuild(code, id, {
          loader: "jsx",
          jsx: "automatic",
        });
      },
    },
    react(),
  ],
  cacheDir: ".vite",
  optimizeDeps: {
    force: true,
    include: ["@intersect.mbo/**/*"],
    esbuildOptions: {
      loader: {
        ".js": "jsx",
      },
    },
  },
  define: {
    "process.env": {},
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
