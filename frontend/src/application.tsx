import React, { ReactElement, Suspense } from "react";
import "~/styles/main.scss";
import { withRedux } from "~/store";
import { Router, View } from "react-navi";
import agoraRouter from "~/router/agoraRouter";
import ErrorPage from "./pages/ErrorPage";

function Application(): ReactElement {
  return (
    <Router routes={agoraRouter()}>
      <ErrorPage>
        <Suspense fallback={null}>
          <View />
        </Suspense>
      </ErrorPage>
    </Router>
  );
}

export default withRedux(Application);
