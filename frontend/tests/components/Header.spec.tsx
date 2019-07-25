import * as React from "react";
import * as renderer from "react-test-renderer";
import "../testUtils/setupTest";
import Header from "../../src/components/common/Header";
import { StaticRouter } from "react-router";

describe("<Header />", (): void => {
  it("renders", (): void => {
    const tree = renderer
      .create(
        <StaticRouter>
          <Header />
        </StaticRouter>
      )
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
});
