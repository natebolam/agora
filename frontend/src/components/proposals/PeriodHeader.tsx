import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import AgoraSelect from "~/components/controls/AgoraSelect";
import ArrowButtonLeft from "~/assets/png/arrow_button_left.png";
import styles from "~/styles/components/proposals/PeriodHeader.scss";
import ProposalStage from "~/components/proposals/ProposalStage";
import ProposalTimeTracker from "~/components/proposals/ProposalTimeTracker";

interface PeriodHeaderTypes {
  className?: string;
  currentStage: "proposal" | "exploration" | "testing" | "promotion";
}

const PeriodHeader: FunctionComponent<PeriodHeaderTypes>
  = ({className, currentStage}): ReactElement => {
  return (
    <div className={cx(className, styles.periodHeader)}>
      <a href="#">
        <img src={ArrowButtonLeft}/>
      </a>
      <AgoraSelect className={styles.periodHeader__selector} options={[{caption: "Period 18", value: "18"}]} />
      <ProposalStage className={styles.periodHeader__stage} stage={currentStage}/>
      <ProposalTimeTracker className={styles.periodHeader__timeTracker}/>
      <a href="#">
        <img src={ArrowButtonLeft}/>
      </a>
    </div>
  )
};

export default PeriodHeader;
