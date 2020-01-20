import React, { ReactElement } from "react";
import { storiesOf } from "@storybook/react";
import { withKnobs, number } from "@storybook/addon-knobs";

import QuorumGraph, { QuorumGraphProps } from "./QuorumGraph";

const stories = storiesOf("Quorum", module);

stories.addDecorator(withKnobs);

const quorumGraphProps: QuorumGraphProps = {
  maxValue: 42,
  value: 21,
  quorumValue: 32,
  quorumMarkLabel: "Quorum",
};

stories.add(
  "No quorum",
  (): ReactElement => <QuorumGraph {...quorumGraphProps} />
);

stories.add(
  "Quorum",
  (): ReactElement => <QuorumGraph {...quorumGraphProps} value={39} />
);

stories.add(
  "Small quorum",
  (): ReactElement => <QuorumGraph {...quorumGraphProps} quorumValue={10} />
);

const valueOptions = {
  range: true,
  min: 0,
  max: 42,
  step: 1,
};

stories.add(
  "Dynamic",
  (): ReactElement => (
    <QuorumGraph
      {...quorumGraphProps}
      value={number("value", 21, valueOptions)}
      quorumValue={number("quorumValue", 32, valueOptions)}
    />
  )
);
