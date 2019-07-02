import React from "react";
import { Route, Switch } from "react-router";
import WelcomePage from "~/pages/WelcomePage";
import ProposalStagePage from "~/pages/proposals/ProposalStagePage.tsx";
import ExplorationStagePage from "~/pages/proposals/ExplorationStagePage";
import PromotionStagePage from "~/pages/proposals/PromotionStagePage";

export default function AgoraRouter(): JSX.Element {
  return (
    <Switch>
      <Route path="/" exact component={WelcomePage}/>
      <Route path="/proposal" component={ProposalStagePage}/>
      <Route path="/exploration" component={ExplorationStagePage}/>
      <Route path="/promotion" component={PromotionStagePage}/>
    </Switch>
  )
}
