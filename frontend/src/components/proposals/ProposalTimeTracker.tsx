import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalTimeTracker.scss";
import { useTranslation } from "react-i18next";

type CircleType = "filled" | "empty" | "current";

interface ProposalTimeCircleTypes {
  type: CircleType;
  circleSize: number;
  borderSize: number;
  cycle?: number;
  current: boolean;
  width: string;
}

const ProposalTimeCircle: FunctionComponent<ProposalTimeCircleTypes> = ({
  type,
  circleSize,
  borderSize,
  current,
  width,
  cycle,
}): ReactElement => {
  const getCircleClassName = (): string => {
    switch (type) {
      case "current":
        return styles.proposalTimeTracker__circle_current;
      case "filled":
        return styles.proposalTimeTracker__circle_filled;
      default:
        return styles.proposalTimeTracker__circle_empty;
    }
  };

  return (
    <div
      className={cx(styles.proposalTimeTracker__circle, getCircleClassName())}
      style={{
        width: circleSize,
        height: circleSize,
        borderWidth: borderSize,
      }}
      title={cycle ? `Cycle ${cycle}` : ""}
    >
      {current && (
        <div
          style={{ width }}
          className={styles.proposalTimeTracker__circle__fill}
        />
      )}
    </div>
  );
};

interface ProposalTimeCirclesTypes {
  className?: string;
  total: number;
  filled: number;
  width: string;
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
  width,
  cycle,
  circleSize = 16,
  borderSize = 2,
}): ReactElement => {
  return (
    <div className={cx(className, styles.proposalTimeTracker__circles)}>
      {new Array(total).fill(0).map(
        (_, index): ReactElement => (
          <ProposalTimeCircle
            type={
              filled == index ? "current" : index < filled ? "filled" : "empty"
            }
            key={index}
            circleSize={circleSize}
            borderSize={borderSize}
            cycle={filled == index ? cycle : void 0}
            current={filled == index}
            width={width}
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
  width: string;
}

const ProposalTimeTracker: FunctionComponent<ProposalTimeTrackerTypes> = ({
  className,
  startDate,
  endDate,
  cycle,
  period,
  width,
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
        width={width}
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
