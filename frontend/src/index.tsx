import React from "react";
import ReactDOM from "react-dom";
import Application from "./application";
import "~/i18n/index";

const root = document.getElementById("root");

ReactDOM.render(<Application />, root);

// Enabling hot module replacement
// Read more about it here:
// https://webpack.js.org/concepts/hot-module-replacement/
if (module.hot) {
  module.hot.accept();
}
