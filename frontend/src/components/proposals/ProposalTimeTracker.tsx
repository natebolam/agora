import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ProposalTimeTracker.scss";
import welcomeStyles from "~/styles/pages/WelcomePage.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";

type CircleType = "filled" | "empty" | "current";

interface ProposalTimeCircleTypes {
  type: CircleType;
  circleSize: number;
  borderSize: number;
  cycle: number;
  current: boolean;
  width: string;
  className?: string;
}

const ProposalTimeCircle: FunctionComponent<ProposalTimeCircleTypes> = ({
  type,
  circleSize,
  borderSize,
  current,
  width,
  className,
}): ReactElement => {
  const getCircleClassName = (): string => {
    if (className === welcomeStyles.welcomePage__stage__timeTracker) {
      switch (type) {
        case "current":
          return styles.proposalTimeTracker__circle_current_welcome;
        case "filled":
          return styles.proposalTimeTracker__circle_filled_welcome;
        default:
          return styles.proposalTimeTracker__circle_empty_welcome;
      }
    } else {
      switch (type) {
        case "current":
          return styles.proposalTimeTracker__circle_current;
        case "filled":
          return styles.proposalTimeTracker__circle_filled;
        default:
          return styles.proposalTimeTracker__circle_empty;
      }
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
    >
      {current && (
        <div
          style={{ width }}
          className={cx(
            {
              [styles.proposalTimeTracker__circle__fill_welcome]:
                className === welcomeStyles.welcomePage__stage__timeTracker,
            },
            styles.proposalTimeTracker__circle__fill
          )}
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
  stage?: number;
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
  stage = 0,
  circleSize = 16,
  borderSize = 2,
}): ReactElement => {
  return (
    <div className={styles.proposalTimeTracker__circles}>
      {new Array(total).fill(0).map(
        (_, index): ReactElement => (
          <ProposalTimeCircle
            type={
              filled == index ? "current" : index < filled ? "filled" : "empty"
            }
            key={index}
            circleSize={circleSize}
            borderSize={borderSize}
            cycle={stage * total + index}
            current={filled == index}
            width={width}
            className={className}
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
  stage: number;
  width: string;
}

const ProposalTimeTracker: FunctionComponent<ProposalTimeTrackerTypes> = ({
  className,
  startDate,
  endDate,
  cycle,
  stage,
  width,
}): ReactElement => {
  const total =
    DateTime.fromISO(endDate).get("day") -
    DateTime.fromISO(startDate).get("day") +
    1;
  const { t } = useTranslation();
  return (
    <div className={cx(className, styles.proposalTimeTracker)}>
      <div
        className={cx(
          {
            [styles.proposalTimeTracker__caption_welcome]:
              className === welcomeStyles.welcomePage__stage__timeTracker,
          },
          styles.proposalTimeTracker__caption
        )}
      >
        {t("proposals.timeTracker.date", {
          value: {
            date: startDate,
            format: "M/d",
          },
        })}
      </div>
      <ProposalTimeCircles
        total={total}
        filled={cycle}
        stage={stage}
        width={width}
        className={className}
      />
      <div
        className={cx(
          {
            [styles.proposalTimeTracker__caption_welcome]:
              className === welcomeStyles.welcomePage__stage__timeTracker,
          },
          styles.proposalTimeTracker__caption
        )}
      >
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
