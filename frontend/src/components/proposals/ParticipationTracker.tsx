import React, {
  FunctionComponent,
  ReactElement,
  createRef,
  RefObject,
} from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ParticipationTracker.scss";
import { useTranslation } from "react-i18next";
import Card from "~/components/common/Card";
import { VoteStats } from "~/models/Stage";

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
  const trackerRef: RefObject<HTMLDivElement> = createRef();

  const participation = (value: number): string =>
    ((value / voteStats.numVotersTotal) * 100).toFixed(2);

  return (
    <Card className={cx(className)} bodyClassName={styles.tracker__body}>
      <div className={styles.tracker__info} ref={trackerRef}>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.participationValue", {
            value: participation(voteStats.numVoters),
          })}
          <span>{t("proposals.participationTracker.participation")}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.totalVotesValue", {
            value: voteStats.numVoters,
            available: voteStats.numVotersTotal,
          })}
          <span>{t("proposals.participationTracker.totalVotes")}</span>
        </div>
      </div>
      <div
        className={cx(styles.tracker__bar, {
          [styles.hidden]: hideProgressBar,
        })}
      >
        <div
          style={{
            width: `${participation(voteStats.numVoters)}%`,
          }}
        />
      </div>
    </Card>
  );
};

export default ParticipationTracker;
