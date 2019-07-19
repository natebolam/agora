import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/PeriodStage.scss";
import { ProposalType } from "~/models/Period";
import AngleIcon from "~/assets/svg/AngleIcon";

interface PeriodStageIndicatorTypes {
  caption: string;
  isCurrent: boolean;
}

const ProposalStageIndicator: FunctionComponent<PeriodStageIndicatorTypes> = ({
  caption,
  isCurrent,
}): ReactElement => {
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

interface PeriodStageTypes {
  className?: string;
  stage: ProposalType;
}

const PeriodStage: FunctionComponent<PeriodStageTypes> = ({
  className,
  stage,
}): ReactElement => {
  return (
    <div className={cx(className, styles.proposalStage)}>
      <ProposalStageIndicator
        caption="Proposal"
        isCurrent={stage === "proposal"}
      />
      <AngleIcon />
      <ProposalStageIndicator
        caption="Exploration"
        isCurrent={stage === "exploration"}
      />
      <AngleIcon />
      <ProposalStageIndicator
        caption="Testing"
        isCurrent={stage === "testing"}
      />
      <AngleIcon />
      <ProposalStageIndicator
        caption="Promotion"
        isCurrent={stage === "promotion"}
      />
    </div>
  );
};

export default PeriodStage;
