import * as React from "react";
import * as renderer from "react-test-renderer";
import "../utils/setupTest";
import Header from "../../src/components/common/Header";

describe("<Header />", (): void => {
  it("renders", (): void => {
    const tree = renderer.create(<Header />).toJSON();
    expect(tree).toMatchSnapshot();
  });
});
