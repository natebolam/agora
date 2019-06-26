import React from "react";
import "~/styles/main.scss";
import { BrowserRouter as Router } from "react-router-dom";
import { withRedux } from "~/store";
import AgoraRouter from "~/router/agoraRouter";

function Application(): JSX.Element {
  return (
    <Router>
      <AgoraRouter/>
    </Router>
  )
}

export default withRedux(Application);