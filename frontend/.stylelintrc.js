module.exports = {
  "extends": "stylelint-config-sass-guidelines",
  "plugins": [
    "stylelint-scss",
  ],
  "rules": {
    "string-quotes": "double",
    "property-no-unknown": [
      true,
      {
        ignoreProperties: ["/^lost-/"],
      },
    ],
    "scss/at-rule-no-unknown": [
      true, { ignoreAtRules: ["lost"] },
    ],
    "selector-pseudo-class-no-unknown": [
      true, { ignorePseudoClasses: ["global"] },
    ],
  },
};
