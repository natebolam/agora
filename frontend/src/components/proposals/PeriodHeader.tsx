import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import AgoraSelect, {
  AgoraSelectDataItem,
} from "~/components/controls/AgoraSelect";
import styles from "~/styles/components/proposals/PeriodHeader.scss";
import {
  PeriodStage,
  PeriodStageShort,
} from "~/components/proposals/PeriodStage";
import ProposalTimeTracker from "~/components/proposals/ProposalTimeTracker";
import { Period, PeriodType } from "~/models/Period";
import { Link } from "react-router-dom";
import useRouter from "use-react-router";
import { useTranslation } from "react-i18next";
import SvgArrow from "~/assets/svg/ArrowIcon";

interface PeriodHeaderTypes {
  className?: string;
  currentStage: PeriodType;
  period: Period;
  totalPeriods: number;
}

const PeriodHeader: FunctionComponent<PeriodHeaderTypes> = ({
  className,
  currentStage,
  period,
  totalPeriods,
}): ReactElement => {
  const { t } = useTranslation();
  const { history } = useRouter();

  const options: AgoraSelectDataItem[] = Array.from(
    Array(totalPeriods),
    (_, index: number): AgoraSelectDataItem => ({
      value: totalPeriods - index - 1,
      caption: t("proposals.periodSelect.caption", {
        value: totalPeriods - index - 1,
      }),
    })
  );

  const value = options[totalPeriods - period.id - 1];

  return (
    <div className={cx(className, styles.periodHeader)}>
      <Link
        to={`/period/${period.id - 1}`}
        className={cx({ [styles.disabled]: period.id === 0 })}
      >
        <div className={styles.periodHeader__arrowIcon}>
          <SvgArrow />
        </div>
      </Link>
      <div className={styles.periodHeader__main}>
        <AgoraSelect
          className={styles.periodHeader__selector}
          options={options}
          value={value}
          onSelect={(newValue: AgoraSelectDataItem): void =>
            history.push(`/period/${newValue.value}`)
          }
        />
        <PeriodStageShort
          className={styles.periodHeader__stage_short}
          stage={currentStage}
        />
      </div>
      <PeriodStage
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
        className={cx({ [styles.disabled]: period.id === totalPeriods - 1 })}
      >
        <div className={styles.periodHeader__arrowIcon}>
          <SvgArrow />
        </div>
      </Link>
    </div>
  );
};

export default PeriodHeader;
