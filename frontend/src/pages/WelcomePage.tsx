import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import ProposalTimeTracker from "~/components/proposals/ProposalTimeTracker";
import { ButtonLink } from "~/components/common/ButtonLink";
import styles from "~/styles/pages/WelcomePage.scss";
import { useTranslation } from "react-i18next";
import { useSelector } from "react-redux";
import { StageType, VoteStats, VotingStageInfo } from "~/models/Stage";
import { RootStoreType } from "~/store";
import SvgUpIcon from "~/assets/svg/UpIcon";
import { Proposal } from "~/models/ProposalInfo";
import { Proposal as ProposalsListItem } from "~/models/ProposalInfo";
import { useLoadingRoute } from "react-navi";
import { Layout } from "~/components/common/Layout";
import BusyIndicator from "react-busy-indicator";
import { DateTime } from "luxon";

interface CurrentStageInfoTypes {
  className?: string;
  currentStageId?: number;
  stageType?: StageType;
  voteStats?: VoteStats;
  proposals?: ProposalsListItem[];
  winner?: Proposal;
}

const CurrentStageInfo: FunctionComponent<CurrentStageInfoTypes> = ({
  className,
  currentStageId,
  stageType,
  proposals,
  winner,
}): ReactElement => {
  const { t } = useTranslation();
  const epoche = Math.floor(currentStageId! / 4 + 1);
  const dayStart = DateTime.local(2020, epoche, (currentStageId! % 4) * 7 + 1);
  const dayEnd =
    dayStart.get("day") === 22
      ? dayStart.endOf("month")
      : dayStart.plus({ days: 7 }).minus(1);

  const currentStageCaption = t(`stageType.${stageType}`);
  const fraction = DateTime.local().get("hour") / 24;
  const width = 100 - (Math.floor(fraction * 4) / 4) * 100 + "%";

  const getStatus = (): JSX.Element | string => {
    if (stageType == "voting" || stageType == "implementation") {
      if (!winner || !proposals) return "";
      const sum = proposals.reduce((a, b): number => a + b.votesCasted, 0);
      const percentage = Math.round((winner.votesCasted / sum) * 10000) / 100;
      return <>{`${winner.title}: ${percentage}%`}</>;
    }

    return "";
  };

  return (
    <div className={cx(className, styles.welcomePage__stage)}>
      <div className={styles.welcomePage__stage__header}>
        {t("welcome.currentStage.header")}
      </div>
      {(stageType == "voting" || stageType == "implementation") && (
        <div className={styles.welcomePage__stage__status}>{getStatus()}</div>
      )}
      <ButtonLink
        className={styles.welcomePage__stage__button}
        href={`/stage/${currentStageId}`}
      >
        {currentStageCaption}
      </ButtonLink>
      <ProposalTimeTracker
        className={styles.welcomePage__stage__timeTracker}
        startDate={dayStart.toISO()}
        endDate={dayEnd.toISO()}
        cycle={dayStart.diffNow().days + 1}
        stage={currentStageId!}
        width={width}
      />
      <div className={styles.welcomePage__stage__remaining}>
        {t("welcome.currentStage.remainingTime", {
          value: {
            date: dayEnd.toISO(),
            options: {
              largest: 1,
            },
          },
        })}
      </div>
    </div>
  );
};

const WelcomePage: FunctionComponent = (): ReactElement => {
  interface StageInfoTypes {
    loading: boolean;
    stageType?: StageType;
    currentStageId?: number;
    discourseLink?: string;
    voteStats?: VoteStats;
    proposals?: ProposalsListItem[];
    winner?: Proposal;
  }

  const stageInfo: StageInfoTypes = useSelector(
    (state: RootStoreType): StageInfoTypes => {
      return {
        loading: state.stageStore.loading,
        stageType: state.stageStore.stage
          ? state.stageStore.stage.type
          : undefined,
        currentStageId: state.stageStore.stage
          ? state.stageStore.stage.stage
          : undefined,
        discourseLink: state.stageStore.stage
          ? state.stageStore.stage.discourseLink
          : undefined,
        voteStats: state.stageStore.stage
          ? (state.stageStore.stage as VotingStageInfo).voteStats
          : undefined,
        proposals: state.stageStore.proposals || undefined,
        winner: state.stageStore.stage
          ? (state.stageStore.stage as VotingStageInfo).winner
          : undefined,
      };
    }
  );

  const loadingRoute = useLoadingRoute();
  return (
    <Layout>
      <BusyIndicator
        active={!!loadingRoute}
        delayMs={0}
        className={""}
        color={"blue"}
        isBusy={!!loadingRoute}
        style={{}}
      />
      <div className={styles.welcomePage_wrapper}>
        <div className={styles.welcomePage}>
          <div className={styles.welcomePage__content}>
            <CurrentStageInfo
              currentStageId={stageInfo.currentStageId}
              stageType={stageInfo.stageType}
              voteStats={stageInfo.voteStats}
              proposals={stageInfo.proposals}
              winner={stageInfo.winner}
            />
          </div>
        </div>
      </div>
    </Layout>
  );
};

export default WelcomePage;
