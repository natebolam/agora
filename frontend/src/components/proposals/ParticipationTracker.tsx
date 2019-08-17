import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ParticipationTracker.scss";
import { useTranslation } from "react-i18next";
import Card from "~/components/common/Card";
import { VoteStats } from "~/models/Period";

interface ParticipationTrackerTypes {
  className?: string;
  voteStats: VoteStats;
  hideProgressBar?: boolean;
}

const ParticipationTracker: FunctionComponent<ParticipationTrackerTypes> = ({
  voteStats,
  hideProgressBar = false,
  className,
}): ReactElement => {
  const { t } = useTranslation();
  const participation: string = (
    (voteStats.votesCast / voteStats.votesAvailable) *
    100
  ).toFixed(2);

  return (
    <Card className={cx(className)} bodyClassName={styles.tracker__body}>
      <div className={styles.tracker__info}>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.totalVotesValue", {
            value: voteStats.votesCast,
          })}
          <span>{t("proposals.participationTracker.totalVotes")}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {participation + "%"}
          <span>{t("proposals.participationTracker.participation")}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.votesAvailableValue", {
            value: voteStats.votesAvailable,
          })}
          <span>{t("proposals.participationTracker.votesAvailable")}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.numVotersValue", {
            value: voteStats.numVoters,
            total: voteStats.numVotersTotal,
          })}
          <span>{t("proposals.participationTracker.numVoters")}</span>
        </div>
      </div>
      <div
        className={cx(styles.tracker__bar, {
          [styles.hidden]: hideProgressBar,
        })}
      >
        <div style={{ width: `${participation}%` }} />
      </div>
    </Card>
  );
};

export default ParticipationTracker;
