import React, { FunctionComponent } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalTimeTracker.scss";

type CircleType = "filled" | "empty";

interface ProposalTimeCircleTypes {
  type: CircleType;
}

const ProposalTimeCircle: FunctionComponent<ProposalTimeCircleTypes>
  = ({type}) => {
  const circleClassName = type == "filled" ?
    styles.proposalTimeTracker__circle_filled
    :
    styles.proposalTimeTracker__circle_empty;

  return (
    <div className={cx(styles.proposalTimeTracker__circle, circleClassName)}>
    </div>
  )
};

interface ProposalTimeCirclesTypes {
  total: number;
  filled: number;
}

const ProposalTimeCircles: FunctionComponent<ProposalTimeCirclesTypes>
  = ({total, filled}) => {
  const circles = new Array(total)
    .fill(0)
    .map((_, index) => index < filled ? "filled" : "empty");
  console.log(circles);
  return (
    <div className={styles.proposalTimeTracker__circles}>
      {
        circles.map((type, index) =>
          <ProposalTimeCircle type={type} key={index}/>
        )
      }
    </div>
  )
};

interface ProposalTimeTrackerTypes {
  className?: string;
}

const ProposalTimeTracker: FunctionComponent<ProposalTimeTrackerTypes>
  = (props) => {
  return (
    <div className={cx(props.className, styles.proposalTimeTracker)}>
      <div className={styles.proposalTimeTracker__caption}>
        8/11
      </div>
      <ProposalTimeCircles total={8} filled={6}/>
      <div className={styles.proposalTimeTracker__caption}>
        5/12
      </div>
    </div>
  );
};

export default ProposalTimeTracker;