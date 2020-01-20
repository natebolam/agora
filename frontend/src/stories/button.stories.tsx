import * as React from "react";
import { storiesOf } from "@storybook/react";
import Wrapper from "./storyWrapper";

import { ButtonLink } from "~/components/common/ButtonLink";
import DiscourseButton from "~/components/controls/DiscourseButton";
import LearnMoreButton from "~/components/controls/LearnMoreButton";

const stories = storiesOf("Buttons", module);

stories.add(
  "Link Buttons",
  (): React.ReactElement => (
    <Wrapper style={{ display: "flex", justifyContent: "space-evenly" }}>
      <ButtonLink href="https://www.google.com">{"A Link Button"}</ButtonLink>
      <DiscourseButton href="discourse-link"></DiscourseButton>
      <LearnMoreButton href="learn-more-link"></LearnMoreButton>
    </Wrapper>
  )
);
