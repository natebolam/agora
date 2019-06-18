import React from "react";
import { Route, Switch } from "react-router";
import WelcomePage from "~/pages/WelcomePage";
import ProposalPage from "~/pages/proposals/ProposalPage";

export default function AgoraRouter(): JSX.Element {
  return (
    <Switch>
      <Route path="/" exact component={WelcomePage}/>
      <Route path="/proposal" component={ProposalPage}/>
    </Switch>
  )
}