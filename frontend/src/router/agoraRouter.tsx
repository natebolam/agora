import React from "react";
import { Route, Switch } from "react-router";
import WelcomePage from "~/pages/WelcomePage";
import PeriodPage from "~/pages/proposals/PeriodPage";
import ProposalInfoPage from "~/pages/proposals/ProposalInfoPage";

export default function AgoraRouter(): JSX.Element {
  return (
    <Switch>
      <Route path="/" exact component={WelcomePage} />
      <Route exact={true} path="/period" component={PeriodPage} />
      <Route path="/period/:id" component={PeriodPage} />
      <Route path="/proposal/:id" component={ProposalInfoPage} />
    </Switch>
  );
}
