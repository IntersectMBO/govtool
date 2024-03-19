module.exports = {
  env: {
    browser: true,
    es2021: true,
  },
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/recommended',
    'plugin:storybook/recommended',
    'plugin:react/recommended',
    'plugin:jest/recommended',
    'airbnb',
  ],
  plugins: ['@typescript-eslint', 'react', 'jest'],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    tsconfigRootDir: __dirname,
    ecmaFeatures: {
      jsx: true,
    },
    ecmaVersion: 2018,
    sourceType: 'module',
    project: './tsconfig.json',
    files: ['*.ts', '*.tsx'],
  },
  rules: {
    quotes: 'off',
    semi: ['error', 'always'],
    'linebreak-style': ['error', 'unix'],
    'no-redeclare': 'off',
    'no-undef': 'off',
    'no-prototype-builtins': 'off',
    'no-unused-vars': 'off',
    'comma-dangle': 'off',
    'operator-linebreak': 'off',

    '@typescript-eslint/no-redeclare': ['error'],
    '@typescript-eslint/no-unused-vars': ['error'],

    'import/prefer-default-export': 'off',
    'import/extensions': 'off',
    'import/no-unresolved': 'off',

    'react/jsx-no-bind': ['warn'],
    'react/jsx-uses-react': ['error'],
    'react/react-in-jsx-scope': 'off',
    'react/display-name': 'off',
    'react/prop-types': 'off',
  },
  ignorePatterns: [
    '.eslintrc.cjs',
    '.storybook/',
    '.vite/',
    'dist/',
    'node_modules/',
  ],
};
