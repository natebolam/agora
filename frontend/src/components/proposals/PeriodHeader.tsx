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
import { Period, PeriodType, PeriodTimeInfo } from "~/models/Period";
import SvgArrow from "~/assets/svg/ArrowIcon";
import { Link, useNavigation } from "react-navi";
import { useTranslation } from "react-i18next";
import { Proposal } from "~/models/ProposalInfo";
import CheckIcon from "~/assets/svg/CheckIcon";
import TimesIcon from "~/assets/svg/TimesIcon";

interface PeriodHeaderTypes {
  className?: string;
  currentStage: PeriodType;
  period: Period;
  totalPeriods: number;
  periodTimes: PeriodTimeInfo;
  proposal: Proposal | null;
  advanced: boolean;
  hideSelected?: boolean;
}

const PeriodHeader: FunctionComponent<PeriodHeaderTypes> = ({
  className,
  currentStage,
  period,
  totalPeriods,
  periodTimes,
  proposal,
  advanced,
  hideSelected,
}): ReactElement => {
  const { t } = useTranslation();
  const navigation = useNavigation();

  const options: AgoraSelectDataItem[] = Array.from(
    Array(totalPeriods),
    (_, index: number): AgoraSelectDataItem => {
      const value = totalPeriods - index - 1;
      return { value, periodTime: periodTimes[value] };
    }
  );

  const value = options[totalPeriods - period.id - 1];
  const fraction = ((period.curLevel + 1) % 4096) / 4096;
  const width = 100 - (Math.floor(fraction * 4) / 4) * 100 + "%";

  const remainingTime = t("welcome.currentPeriod.remainingTime", {
    value: {
      date: period.endTime,
      options: {
        largest: 1,
      },
    },
  });

  return (
    <div className={cx(className, styles.periodHeader)}>
      <Link
        href={`/period/${period.id - 1}`}
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
          onSelect={(newValue: AgoraSelectDataItem): void => {
            navigation.navigate(`/period/${newValue.value}`);
          }}
        />
        <PeriodStageShort
          className={styles.periodHeader__stage_short}
          periodId={period.id}
          stage={currentStage}
          periodTimes={periodTimes}
        />
      </div>
      <PeriodStage
        className={styles.periodHeader__stage}
        stage={currentStage}
        periodId={period.id}
        periodTimes={periodTimes}
        hideSelected={hideSelected}
      />
      <ProposalTimeTracker
        className={styles.periodHeader__timeTracker}
        startDate={period.startTime}
        endDate={period.endTime}
        cycle={period.cycle}
        period={period.id}
        width={width}
      />
      {period.id === totalPeriods - 1 ? (
        <div className={styles.periodHeader__timeRemaining}>
          {remainingTime}
        </div>
      ) : (
        proposal && (
          <div className={styles.periodHeader__winner}>
            {proposal.title}
            {advanced ? <CheckIcon /> : <TimesIcon />}
          </div>
        )
      )}
      <Link
        href={`/period/${period.id + 1}`}
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
