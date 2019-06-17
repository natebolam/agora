module.exports = {
  "extends": "stylelint-config-standard",
  "plugins": [
    "stylelint-scss",
  ],
  "rules": {
    "selector-pseudo-class-no-unknown": [true, {
      "ignorePseudoClasses": ["global"]
    }]
  }
};
