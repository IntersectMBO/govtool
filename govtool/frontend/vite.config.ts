import path from "path";
import { defineConfig as defineViteConfig, mergeConfig } from "vite";
import { defineConfig as defineVitestConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import babel from "vite-plugin-babel";

// https://vitejs.dev/config/
const viteConfig = defineViteConfig({
  plugins: [
    react(),
    // Transpile @intersect.mbo packages as they provides jsx in js which is not supported in vite.
    babel({
      include: [
        "node_modules/@intersect.mbo/**/*.js",
        "node_modules/@intersect.mbo/**/*.jsx",
      ],
      exclude: /node_modules\/@intersect\.mbo\/.*\/node_modules\/.*/, // Exclude nested node_modules
      babelConfig: {
        presets: ["@babel/preset-react"],
      },
    }),
  ],
  cacheDir: ".vite",
  // Required for the @intersect.mbo packages as they uses the .env from process which is not supported in vite.
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
    // coverage: {
    //   include: [
    //     "src/components/**/*",
    //     "src/consts/**/*",
    //     "src/context/**/*",
    //     "src/hooks/**/*",
    //     "src/services/**/*",
    //     "src/utils/**/*",
    //   ],
    //   provider: "v8",
    //   reporter: ["json-summary", "lcov"],
    //   reportOnFailure: true,
    //   enabled: true,
    // },
  },
});

export default mergeConfig(viteConfig, vitestConfig);
