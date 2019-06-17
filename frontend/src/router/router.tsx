import React from "react";
import { Route, Switch } from "react-router";
import WelcomePage from "~/pages/WelcomePage";

export default function Router(): JSX.Element {
  return (
    <Switch>
      <Route path="/" exact component={WelcomePage}/>
    </Switch>
  )
}