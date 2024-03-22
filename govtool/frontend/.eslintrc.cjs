module.exports = {
  env: {
    browser: true,
    es2021: true,
  },
  extends: [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:storybook/recommended",
    "plugin:react/recommended",
    "plugin:react-hooks/recommended",
    "plugin:jest/recommended",
    "airbnb",
  ],
  plugins: ["@typescript-eslint", "react", "jest"],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    tsconfigRootDir: __dirname,
    ecmaFeatures: {
      jsx: true,
    },
    ecmaVersion: 2018,
    sourceType: "module",
    project: "./tsconfig.json",
    files: ["*.ts", "*.tsx"],
  },
  rules: {
    "array-callback-return": "off",
    indent: "off",
    quotes: "off",
    semi: ["error", "always"],
    "linebreak-style": ["error", "unix"],
    "no-redeclare": "off",
    "no-undef": "off",
    "no-prototype-builtins": "off",

    // TODO: This rule should be enabled in the future
    "no-param-reassign": "off",

    // TODO: This rule should be enabled in the future
    "no-plusplus": "off",

    // TODO: This rule should be enabled in the future
    "no-nested-ternary": "off",

    "no-unused-vars": "off",
    "no-use-before-define": "off",
    "comma-dangle": "off",
    "operator-linebreak": "off",
    "implicit-arrow-linebreak": "off",
    "consistent-return": "off",
    "no-shadow": "off",
    "function-paren-newline": "off",
    "object-curly-newline": "off",

    "@typescript-eslint/no-shadow": ["error"],
    "@typescript-eslint/no-redeclare": ["error"],
    "@typescript-eslint/no-unused-vars": ["error"],

    "import/prefer-default-export": "off",
    "import/extensions": "off",
    "import/no-unresolved": "off",
    "import/no-extraneous-dependencies": [
      "error",
      {
        devDependencies: ["**/*.stories.{ts,tsx}", "**/*.test.{ts,tsx}"],
      },
    ],

    "jsx-a11y/anchor-is-valid": "off",
    "jsx-a11y/no-noninteractive-element-interactions": "off",
    "jsx-a11y/click-events-have-key-events": "off",

    "react/display-name": "off",
    "react/jsx-curly-newline": "off",
    "react/jsx-filename-extension": ["error", { extensions: [".tsx"] }],
    "react/jsx-fragments": "off",
    "react/jsx-no-bind": "warn",
    "react/jsx-no-useless-fragment": "off",
    "react/jsx-wrap-multilines": "off",
    "react/jsx-props-no-spreading": "off",
    "react/jsx-uses-react": "error",
    "react/jsx-one-expression-per-line": "off",
    "react/prop-types": "off",
    "react/react-in-jsx-scope": "off",
    "react/function-component-definition": [
      "error",
      {
        namedComponents: "arrow-function",
        unnamedComponents: "arrow-function",
      },
    ],
    "react/require-default-props": "off",

    // TODO: This rule should be enabled in the future
    "react-hooks/exhaustive-deps": "off",
  },
  ignorePatterns: [
    ".eslintrc.cjs",
    ".storybook/",
    ".vite/",
    "dist/",
    "node_modules/",
    "vite.config.ts",
  ],
};
