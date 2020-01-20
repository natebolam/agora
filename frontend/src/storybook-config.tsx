import { configure, addParameters } from "@storybook/react";
import { themes } from "@storybook/theming";

const loadStories = (): void => {
  require("./stories/**/*.stories.tsx");
  require("./components/**/*.stories.tsx");
};

/** Using storybook standalone build with parcel bypasses normal storybook
 * method of resolving aliases (i.e. with custom webpack.config.js). Keeping
 * these files in src folder instead of .storybook folder until that is fixed is a
 * temporary solution.
 */

addParameters({
  options: {
    theme: themes.light,
  },
});

configure(loadStories, module);
