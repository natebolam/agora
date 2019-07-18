module.exports = {
  parser: '@typescript-eslint/parser',
  extends: [
    'plugin:react/recommended',
    'plugin:@typescript-eslint/recommended',
    'prettier/@typescript-eslint',
    'plugin:prettier/recommended',

  ],
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: 'module',
  },
  rules: {
    indent: ["error", 2, { SwitchCase: 1 }],
    "no-unused-vars": ["warn", { "vars": "all", "args": "after-used" }],
    "react/display-name": 0,
    "react/prop-types": 0,
    "react/jsx-no-literals": ["warn"]
  },
  settings: {
    react: {
      version: 'detect',
    },
  },
};
