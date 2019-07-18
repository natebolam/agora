import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalTimeTracker.scss";
import { useTranslation } from "react-i18next";

type CircleType = "filled" | "empty";

interface ProposalTimeCircleTypes {
  type: CircleType;
}

const ProposalTimeCircle: FunctionComponent<ProposalTimeCircleTypes> = ({
  type,
}): ReactElement => {
  const circleClassName =
    type == "filled"
      ? styles.proposalTimeTracker__circle_filled
      : styles.proposalTimeTracker__circle_empty;

  return (
    <div className={cx(styles.proposalTimeTracker__circle, circleClassName)} />
  );
};

interface ProposalTimeCirclesTypes {
  total: number;
  filled: number;
}

const ProposalTimeCircles: FunctionComponent<ProposalTimeCirclesTypes> = ({
  total,
  filled,
}): ReactElement => {
  const circles = new Array(total)
    .fill(0)
    .map((_, index): CircleType => (index < filled ? "filled" : "empty"));

  return (
    <div className={styles.proposalTimeTracker__circles}>
      {circles.map(
        (type, index): ReactElement => (
          <ProposalTimeCircle type={type} key={index} />
        )
      )}
    </div>
  );
};

interface ProposalTimeTrackerTypes {
  className?: string;
}

const ProposalTimeTracker: FunctionComponent<ProposalTimeTrackerTypes> = (
  props
): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={cx(props.className, styles.proposalTimeTracker)}>
      <div className={styles.proposalTimeTracker__caption}>
        {t("proposals.timeTracker.date", {
          value: {
            date: new Date().toISOString(),
            format: "dd/MM",
          },
        })}
      </div>
      <ProposalTimeCircles total={8} filled={6} />
      <div className={styles.proposalTimeTracker__caption}>
        {t("proposals.timeTracker.date", {
          value: {
            date: new Date().toISOString(),
            format: "dd/MM",
          },
        })}
      </div>
    </div>
  );
};

export default ProposalTimeTracker;
