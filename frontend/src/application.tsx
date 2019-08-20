import React, { ReactElement, Suspense } from "react";
import "~/styles/main.scss";
import { withRedux } from "~/store";
import { Router, View } from "react-navi";
import agoraRouter from "~/router/agoraRouter";

function Application(): ReactElement {
  return (
    <Router routes={agoraRouter()}>
      <Suspense fallback={null}>
        <View />
      </Suspense>
    </Router>
  );
}

export default withRedux(Application);
