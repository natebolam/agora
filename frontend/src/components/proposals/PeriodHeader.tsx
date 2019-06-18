import React, { FunctionComponent } from "react";
import AgoraSelect from "~/components/controls/AgoraSelect";
import ArrowButtonLeft from "~/assets/png/arrow_button_left.png";
import styles from "~/styles/components/proposals/PeriodHeader.scss";
import ProposalStage from "~/components/proposals/ProposalStage";
import ProposalTimeTracker from "~/components/proposals/ProposalTimeTracker";

const PeriodHeader: FunctionComponent = () => {
  return (
    <div className={styles.periodHeader}>
      <a href="#">
        <img src={ArrowButtonLeft}/>
      </a>
      <AgoraSelect className={styles.periodHeader__selector} options={[{caption: "Period 18", value: "18"}]} />
      <ProposalStage className={styles.periodHeader__stage} stage={"proposal"}/>
      <ProposalTimeTracker className={styles.periodHeader__timeTracker}/>
      <a href="#">
        <img src={ArrowButtonLeft}/>
      </a>
    </div>
  )
};

export default PeriodHeader;
