import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalTimeTracker.scss";
import { useTranslation } from "react-i18next";

type CircleType = "filled" | "empty";

interface ProposalTimeCircleTypes {
  type: CircleType;
  circleSize: number;
  borderSize: number;
}

const ProposalTimeCircle: FunctionComponent<ProposalTimeCircleTypes> = ({
  type,
  circleSize,
  borderSize,
}): ReactElement => {
  const circleClassName =
    type == "filled"
      ? styles.proposalTimeTracker__circle_filled
      : styles.proposalTimeTracker__circle_empty;

  return (
    <div
      className={cx(styles.proposalTimeTracker__circle, circleClassName)}
      style={{
        width: circleSize,
        height: circleSize,
        borderWidth: borderSize,
      }}
    />
  );
};

interface ProposalTimeCirclesTypes {
  className?: string;
  total: number;
  filled: number;
  circleSize?: number;
  borderSize?: number;
}

export const ProposalTimeCircles: FunctionComponent<
  ProposalTimeCirclesTypes
> = ({
  className,
  total,
  filled,
  circleSize = 16,
  borderSize = 2,
}): ReactElement => {
  const circles = new Array(total)
    .fill(0)
    .map((_, index): CircleType => (index < filled ? "filled" : "empty"));

  return (
    <div className={cx(className, styles.proposalTimeTracker__circles)}>
      {circles.map(
        (type, index): ReactElement => (
          <ProposalTimeCircle
            type={type}
            key={index}
            circleSize={circleSize}
            borderSize={borderSize}
          />
        )
      )}
    </div>
  );
};

interface ProposalTimeTrackerTypes {
  className?: string;
  startDate: string;
  endDate: string;
  cycle: number;
}

const ProposalTimeTracker: FunctionComponent<ProposalTimeTrackerTypes> = ({
  className,
  startDate,
  endDate,
  cycle,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={cx(className, styles.proposalTimeTracker)}>
      <div className={styles.proposalTimeTracker__caption}>
        {t("proposals.timeTracker.date", {
          value: {
            date: startDate,
            format: "dd/MM",
          },
        })}
      </div>
      <ProposalTimeCircles total={8} filled={cycle} />
      <div className={styles.proposalTimeTracker__caption}>
        {t("proposals.timeTracker.date", {
          value: {
            date: endDate,
            format: "dd/MM",
          },
        })}
      </div>
    </div>
  );
};

export default ProposalTimeTracker;
