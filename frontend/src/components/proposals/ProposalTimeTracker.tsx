import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalTimeTracker.scss";
import { useTranslation } from "react-i18next";

type CircleType = "filled" | "empty";

interface ProposalTimeCircleTypes {
  type: CircleType;
  circleSize: number;
  borderSize: number;
  cycle?: number;
}

const ProposalTimeCircle: FunctionComponent<ProposalTimeCircleTypes> = ({
  type,
  circleSize,
  borderSize,
  cycle,
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
      title={cycle ? `Cycle ${cycle}` : ""}
      data-cycle={cycle}
    />
  );
};

interface ProposalTimeCirclesTypes {
  className?: string;
  total: number;
  filled: number;
  cycle?: number;
  circleSize?: number;
  borderSize?: number;
}

export const ProposalTimeCircles: FunctionComponent<
  ProposalTimeCirclesTypes
> = ({
  className,
  total,
  filled,
  cycle,
  circleSize = 16,
  borderSize = 2,
}): ReactElement => {
  const circles = new Array(total)
    .fill(0)
    .map((_, index): CircleType => (index < filled ? "filled" : "empty"));

  const cycleIndex = circles.indexOf("empty");

  return (
    <div className={cx(className, styles.proposalTimeTracker__circles)}>
      {circles.map(
        (type, index): ReactElement => (
          <ProposalTimeCircle
            type={type}
            key={index}
            circleSize={circleSize}
            borderSize={borderSize}
            cycle={cycle && cycleIndex == index ? cycle : void 0}
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
  period: number;
}

const ProposalTimeTracker: FunctionComponent<ProposalTimeTrackerTypes> = ({
  className,
  startDate,
  endDate,
  cycle,
  period,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={cx(className, styles.proposalTimeTracker)}>
      <div className={styles.proposalTimeTracker__caption}>
        {t("proposals.timeTracker.date", {
          value: {
            date: startDate,
            format: "M/d",
          },
        })}
      </div>
      <ProposalTimeCircles
        total={8}
        filled={cycle}
        cycle={period * 8 + cycle}
      />
      <div className={styles.proposalTimeTracker__caption}>
        {t("proposals.timeTracker.date", {
          value: {
            date: endDate,
            format: "M/d",
          },
        })}
      </div>
    </div>
  );
};

export default ProposalTimeTracker;
