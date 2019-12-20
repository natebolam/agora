import { configure } from '@storybook/react';

const loadStories = () => {
  require('../stories/**/*.stories.tsx');
}

/** Using storybook standalone build with parcel bypasses normal storybook 
 * method of resolving aliases (i.e. with custom webpack.config.js). Keeping
 * these files in src folder instead of .storybook folder until that is fixed is a 
 * temporary solution.
 */

configure(loadStories, module);