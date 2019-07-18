import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalStage.scss";
import ArrowRight from "~/assets/png/arrow_right.png";

interface ProposalStageIndicatorTypes {
  caption: string;
  isCurrent: boolean;
}

const ProposalStageIndicator: FunctionComponent<
  ProposalStageIndicatorTypes
> = ({ caption, isCurrent }): ReactElement => {
  return (
    <div
      className={cx(styles.proposalStage__indicator, {
        [styles.proposalStage__indicator_selected]: isCurrent,
      })}
    >
      {caption}
    </div>
  );
};

interface ProposalStageTypes {
  className?: string;
  stage: "proposal" | "exploration" | "testing" | "promotion";
}

const ProposalStage: FunctionComponent<ProposalStageTypes> = ({
  className,
  stage,
}): ReactElement => {
  return (
    <div className={cx(className, styles.proposalStage)}>
      <ProposalStageIndicator
        caption="Proposal"
        isCurrent={stage === "proposal"}
      />
      <img alt="" src={ArrowRight} />
      <ProposalStageIndicator
        caption="Exploration"
        isCurrent={stage === "exploration"}
      />
      <img alt="" src={ArrowRight} />
      <ProposalStageIndicator
        caption="Testing"
        isCurrent={stage === "testing"}
      />
      <img alt="" src={ArrowRight} />
      <ProposalStageIndicator
        caption="Promotion"
        isCurrent={stage === "promotion"}
      />
    </div>
  );
};

export default ProposalStage;
