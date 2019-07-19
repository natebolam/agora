import React, { FunctionComponent, ReactElement, ReactNode } from "react";
import cx from "classnames";
import AgoraSelect from "~/components/controls/AgoraSelect";
import ArrowButtonLeft from "~/assets/png/arrow_button_left.png";
import styles from "~/styles/components/proposals/PeriodHeader.scss";
import ProposalStage from "~/components/proposals/ProposalStage";
import ProposalTimeTracker from "~/components/proposals/ProposalTimeTracker";
import { Period, ProposalType } from "~/models/Period";
import { Link } from "react-router-dom";

interface PeriodHeaderTypes {
  className?: string;
  currentStage: ProposalType;
  period: Period;
  totalPeriods: number;
}

const PeriodHeader: FunctionComponent<PeriodHeaderTypes> = ({
  className,
  currentStage,
  period,
  totalPeriods,
}): ReactElement => {
  const options: Record<string, ReactNode>[] = [
    {
      caption: `${period.id} period`,
    },
  ];

  return (
    <div className={cx(className, styles.periodHeader)}>
      <Link
        to={`/period/${period.id - 1}`}
        className={cx({ [styles.diszabled]: period.id === 1 })}
      >
        <img alt="" src={ArrowButtonLeft} />
      </Link>
      <AgoraSelect
        className={styles.periodHeader__selector}
        options={options}
      />
      <ProposalStage
        className={styles.periodHeader__stage}
        stage={currentStage}
      />
      <ProposalTimeTracker
        className={styles.periodHeader__timeTracker}
        startDate={period.startTime}
        endDate={period.endTime}
        cycle={period.cycle}
      />
      <Link
        to={`/period/${period.id + 1}`}
        className={cx({ [styles.disabled]: period.id === totalPeriods })}
      >
        <img alt="" src={ArrowButtonLeft} />
      </Link>
    </div>
  );
};

export default PeriodHeader;
