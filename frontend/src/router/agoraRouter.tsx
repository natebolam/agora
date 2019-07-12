import React from "react";
import { Route, Switch } from "react-router";
import WelcomePage from "~/pages/WelcomePage";
import ProposalStagePage from "~/pages/proposals/ProposalStagePage";
import ExplorationStagePage from "~/pages/proposals/ExplorationStagePage";
import PromotionStagePage from "~/pages/proposals/PromotionStagePage";
import TestingStagePage from "~/pages/proposals/TestingStagePage";

export default function AgoraRouter(): JSX.Element {
  return (
    <Switch>
      <Route path="/" exact component={WelcomePage} />
      <Route path="/proposal" component={ProposalStagePage} />
      <Route path="/exploration" component={ExplorationStagePage} />
      <Route path="/promotion" component={PromotionStagePage} />
      <Route path="/testing" component={TestingStagePage} />
    </Switch>
  );
}
