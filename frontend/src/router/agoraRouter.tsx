import React from "react";
import { Route, Switch } from "react-router";
import WelcomePage from "~/pages/WelcomePage";
import PeriodPage from "~/pages/proposals/PeriodPage";

export default function AgoraRouter(): JSX.Element {
  return (
    <Switch>
      <Route path="/" exact component={WelcomePage} />
      <Route exact={true} path="/period" component={PeriodPage} />
      <Route path="/period/:id" component={PeriodPage} />
    </Switch>
  );
}
