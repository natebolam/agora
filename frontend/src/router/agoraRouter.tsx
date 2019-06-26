import React from "react";
import { Route, Switch } from "react-router";
import WelcomePage from "~/pages/WelcomePage";
import ProposalStagePage from "~/pages/proposals/ProposalStagePage.tsx";

export default function AgoraRouter(): JSX.Element {
  return (
    <Switch>
      <Route path="/" exact component={WelcomePage}/>
      <Route path="/proposal" component={ProposalStagePage}/>
    </Switch>
  )
}