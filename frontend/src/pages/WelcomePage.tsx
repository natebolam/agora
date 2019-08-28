import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import { ProposalTimeCircles } from "~/components/proposals/ProposalTimeTracker";
import { ButtonLink } from "~/components/common/ButtonLink";
import styles from "~/styles/pages/WelcomePage.scss";
import { useTranslation } from "react-i18next";
import { useSelector } from "react-redux";
import {
  PeriodType,
  BallotsStats,
  ExplorationPeriodInfo,
  ProposalPeriodInfo,
  VoteStats,
} from "~/models/Period";
import { RootStoreType } from "~/store";
import Logo from "~/assets/svg/Logo";
import TimesIcon from "~/assets/svg/TimesIcon";
import CheckIcon from "~/assets/svg/CheckIcon";
import SvgUpIcon from "~/assets/svg/UpIcon";
import { Proposal } from "~/models/ProposalInfo";
import { ProposalsListItem } from "~/models/ProposalsList";

const WelcomePageHeader: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();

  return (
    <div className={styles.welcomePage__header}>
      <Logo />
      <div className={styles.welcomePage__header__caption}>
        {t("header.logoCaption")}
      </div>
    </div>
  );
};

interface AgoraLinksTypes {
  discourseLink?: string;
}

const AgoraLinks: FunctionComponent<AgoraLinksTypes> = ({
  discourseLink,
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <div className={styles.welcomePage__links}>
      <a href={t("tezosLinks.learnLink")}>
        <div className={styles.welcomePage__links__header}>
          {t("welcome.links.learnHeader")}
        </div>
        <div className={styles.welcomePage__links__content}>
          {t("welcome.links.learnDescription")}
        </div>
      </a>
      <a href={discourseLink}>
        <div className={styles.welcomePage__links__header}>
          {t("welcome.links.discussHeader")}
        </div>
        <div className={styles.welcomePage__links__content}>
          {t("welcome.links.discussDescription")}
        </div>
      </a>
      <a href={t("tezosLinks.getStartedLink")}>
        <div className={styles.welcomePage__links__header}>
          {t("welcome.links.getStartedHeader")}
        </div>
        <div className={styles.welcomePage__links__content}>
          {t("welcome.links.getStartedDescription")}
        </div>
      </a>
    </div>
  );
};

interface CurrentPeriodInfoTypes {
  className?: string;
  currentPeriodId?: number;
  currentCycle?: number;
  periodType?: PeriodType;
  timeRemaining?: string;
  curLevel?: number;
  ballots?: BallotsStats;
  voteStats?: VoteStats;
  proposals?: ProposalsListItem[];
  winner?: Proposal;
}

const CurrentPeriodInfo: FunctionComponent<CurrentPeriodInfoTypes> = ({
  className,
  currentPeriodId,
  currentCycle = 0,
  periodType,
  timeRemaining,
  curLevel = 0,
  ballots,
  voteStats,
  proposals,
  winner,
}): ReactElement => {
  const { t } = useTranslation();

  const currentPeriodCaption = t(`periodType.${periodType}`);
  const fraction = ((curLevel + 1) % 4096) / 4096;
  const width = 100 - (Math.floor(fraction * 4) / 4) * 100 + "%";

  const getStatus = (): JSX.Element | string => {
    if (periodType == "testing") return "";
    if (periodType == "proposal") {
      if (!winner || !proposals) return "";
      const sum = proposals.reduce((a, b): number => a + b.votesCasted, 0);
      const percentage = Math.round((winner.votesCasted / sum) * 10000) / 100;
      return (
        <>
          {`${winner.title}: ${percentage}%`} <SvgUpIcon />
        </>
      );
    }

    if (!ballots || !voteStats) return "";

    const percentage =
      Math.round((ballots.yay / (ballots.yay + ballots.nay)) * 10000) / 100;
    const isQuorum =
      (voteStats.votesCast / voteStats.votesAvailable) * 100 >= ballots.quorum;

    return (
      <>
        {`${percentage}%`} <SvgUpIcon />
        {" Quorum "}
        {isQuorum ? <CheckIcon /> : <TimesIcon />}
      </>
    );
  };

  return (
    <div className={cx(className, styles.welcomePage__period)}>
      <div className={styles.welcomePage__period__header}>
        {t("welcome.currentPeriod.header")}
      </div>
      <div className={styles.welcomePage__period__status}>{getStatus()}</div>
      <ButtonLink
        className={styles.welcomePage__period__button}
        href={`/period/${currentPeriodId}`}
        // prefetch
      >
        {currentPeriodCaption}
      </ButtonLink>
      <ProposalTimeCircles
        className={styles.welcomePage__period__timeTracker}
        total={8}
        filled={currentCycle}
        circleSize={30}
        borderSize={4}
        cycle={currentPeriodId ? currentPeriodId * 8 + currentCycle : 0}
        width={width}
      />
      <div className={styles.welcomePage__period__remaining}>
        {t("welcome.currentPeriod.remainingTime", {
          value: {
            date: timeRemaining,
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
  interface PeriodInfoTypes {
    loading: boolean;
    periodType?: PeriodType;
    endTime?: string;
    currentPeriodId?: number;
    currentCycle?: number;
    discourseLink?: string;
    curLevel?: number;
    ballots?: BallotsStats;
    voteStats?: VoteStats;
    proposals?: ProposalsListItem[];
    winner?: Proposal;
  }

  const periodInfo: PeriodInfoTypes = useSelector(
    (state: RootStoreType): PeriodInfoTypes => {
      return {
        loading: state.periodStore.loading,
        periodType: state.periodStore.period
          ? state.periodStore.period.type
          : undefined,
        endTime: state.periodStore.period
          ? state.periodStore.period.period.endTime
          : undefined,
        currentPeriodId: state.periodStore.period
          ? state.periodStore.period.period.id
          : undefined,
        currentCycle: state.periodStore.period
          ? state.periodStore.period.period.cycle
          : undefined,
        discourseLink: state.periodStore.period
          ? state.periodStore.period.discourseLink
          : undefined,
        curLevel: state.periodStore.period
          ? state.periodStore.period.period.curLevel
          : undefined,
        ballots: state.periodStore.period
          ? (state.periodStore.period as ExplorationPeriodInfo).ballots
          : undefined,
        voteStats: state.periodStore.period
          ? (state.periodStore.period as ExplorationPeriodInfo).voteStats
          : undefined,
        proposals: state.periodStore.proposals || undefined,
        winner: state.periodStore.period
          ? (state.periodStore.period as ProposalPeriodInfo).winner
          : undefined,
      };
    }
  );

  return (
    <div className={styles.welcomePage_wrapper}>
      <div className={styles.welcomePage}>
        <WelcomePageHeader />
        <div className={styles.welcomePage__content}>
          <AgoraLinks discourseLink={periodInfo.discourseLink} />
          <CurrentPeriodInfo
            currentPeriodId={periodInfo.currentPeriodId}
            periodType={periodInfo.periodType}
            timeRemaining={periodInfo.endTime}
            currentCycle={periodInfo.currentCycle}
            curLevel={periodInfo.curLevel}
            ballots={periodInfo.ballots}
            voteStats={periodInfo.voteStats}
            proposals={periodInfo.proposals}
            winner={periodInfo.winner}
          />
        </div>
      </div>
    </div>
  );
};

export default WelcomePage;
