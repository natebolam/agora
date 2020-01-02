import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import AgoraSelect, {
  AgoraSelectDataItem,
} from "~/components/controls/AgoraSelect";
import styles from "~/styles/components/proposals/StageHeader.scss";
import { StageStage, StageStageShort } from "~/components/proposals/StageStage";
import ProposalTimeTracker from "~/components/proposals/ProposalTimeTracker";
import { StageType, StageTimeInfo } from "~/models/Stage";
import SvgArrow from "~/assets/svg/ArrowIcon";
import { Link, useNavigation } from "react-navi";
import { useTranslation } from "react-i18next";
import { Proposal } from "~/models/ProposalInfo";
import CheckIcon from "~/assets/svg/CheckIcon";
import TimesIcon from "~/assets/svg/TimesIcon";
import { DateTime } from "luxon";

interface StageHeaderTypes {
  className?: string;
  currentStage: StageType;
  stage: number;
  totalStages: number;
  stageTimes: StageTimeInfo;
  proposal: Proposal | null;
  hideSelected?: boolean;
}

const StageHeader: FunctionComponent<StageHeaderTypes> = ({
  className,
  currentStage,
  stage,
  totalStages,
  stageTimes,
  proposal,
  hideSelected,
}): ReactElement => {
  const { t } = useTranslation();
  const navigation = useNavigation();
  const epoche = Math.floor(stage / 4 + 1);
  const dayStart = DateTime.local(2020, epoche, (stage % 4) * 7 + 1);
  const dayEnd =
    dayStart.get("day") === 22
      ? dayStart.endOf("month")
      : dayStart.plus({ days: 7 }).minus(1);

  const options: AgoraSelectDataItem[] = Array.from(
    Array(totalStages),
    (_, index: number): AgoraSelectDataItem => {
      const value = totalStages - index - 1;
      const currentStage = stageTimes[value].stage;
      const currentEpoche = Math.floor(currentStage / 4 + 1);
      const startTime = DateTime.local(
        2020,
        currentEpoche,
        currentStage * 7 + 1
      );
      const endTime =
        startTime.get("day") === 22
          ? startTime.endOf("month")
          : startTime.plus({ days: 7 }).minus(1);
      return {
        value,
        stageType: stageTimes[value].stageType,
        startTime: startTime.toISO(),
        endTime: endTime.toISO(),
      };
    }
  );

  const value = options[totalStages - stage - 1];

  const fraction = DateTime.local().get("hour") / 24;
  const width = 100 - (Math.floor(fraction * 4) / 4) * 100 + "%";

  const remainingTime = t("welcome.currentStage.remainingTime", {
    value: {
      date: dayEnd.toISO(),
      options: {
        largest: 1,
      },
    },
  });

  return (
    <div className={cx(className, styles.stageHeader)}>
      <Link
        href={`/stage/${stage - 1}`}
        className={cx({ [styles.disabled]: stage === 0 })}
      >
        <div className={styles.stageHeader__arrowIcon}>
          <SvgArrow />
        </div>
      </Link>
      <div className={styles.stageHeader__main}>
        <AgoraSelect
          className={styles.stageHeader__selector}
          options={options}
          value={value}
          onSelect={(newValue: AgoraSelectDataItem): void => {
            navigation.navigate(`/stage/${newValue.value}`);
          }}
        />
        <StageStageShort
          className={styles.stageHeader__stage_short}
          stageId={stage}
          stage={currentStage}
          stageTimes={stageTimes}
        />
      </div>
      <StageStage
        className={styles.stageHeader__stage}
        stage={currentStage}
        stageId={stage}
        stageTimes={stageTimes}
        hideSelected={hideSelected}
      />
      <ProposalTimeTracker
        className={styles.stageHeader__timeTracker}
        startDate={dayStart.toISO()}
        endDate={dayEnd.toISO()}
        cycle={dayStart.diffNow().days + 1}
        stage={stage}
        width={width}
      />
      {stage === totalStages - 1 ? (
        <div className={styles.stageHeader__timeRemaining}>{remainingTime}</div>
      ) : (
        proposal && (
          <div className={styles.stageHeader__winner}>
            {proposal.title}
            {false ? <CheckIcon /> : <TimesIcon />}
          </div>
        )
      )}
      <Link
        href={`/stage/${stage + 1}`}
        className={cx({ [styles.disabled]: stage === totalStages - 1 })}
      >
        <div className={styles.stageHeader__arrowIcon}>
          <SvgArrow />
        </div>
      </Link>
    </div>
  );
};

export default StageHeader;
