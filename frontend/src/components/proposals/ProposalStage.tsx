import React, { FunctionComponent } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalStage";
import ArrowRight from "~/assets/png/arrow_right.png";

interface ProposalStageIndicatorTypes {
  caption: string;
  isCurrent: boolean;
}

const ProposalStageIndicator: FunctionComponent<ProposalStageIndicatorTypes>
  = ({caption, isCurrent}) => {
  return (
    <div className={cx(styles.proposalStage__indicator, {[styles.proposalStage__indicator_selected]: isCurrent})}>
      {caption}
    </div>
  )
};

interface ProposalStageTypes {
  className?: string;
  stage: "proposal" | "exploration" | "testing" | "promotion";
}

const ProposalStage: FunctionComponent<ProposalStageTypes>
  = ({className, stage}) => {
  return (
    <div className={cx(className, styles.proposalStage)}>
      <ProposalStageIndicator caption="Proposal" isCurrent={false}/>
      <img src={ArrowRight}/>
      <ProposalStageIndicator caption="Exploration" isCurrent={true}/>
      <img src={ArrowRight}/>
      <ProposalStageIndicator caption="Testing" isCurrent={false}/>
      <img src={ArrowRight}/>
      <ProposalStageIndicator caption="Promotion" isCurrent={false}/>
    </div>
  )
};

export default ProposalStage;