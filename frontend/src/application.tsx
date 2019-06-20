import React from "react";
import WelcomePage from "~/pages/WelcomePage";
import "~/styles/main.scss";
import { BrowserRouter as Router } from "react-router-dom";
import { withRedux } from "~/store";

function Application(): JSX.Element {
  return (
    <Router>
      <WelcomePage/>
    </Router>
  )
}

export default withRedux(Application);