import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ParticipationTracker.scss";
import { useTranslation } from "react-i18next";

interface ParticipationTrackerTypes {
  className?: string;
  totalVotes: number;
  participation: number;
  availableVotes: number;
  hideProgressBar?: boolean;
}

const ParticipationTracker: FunctionComponent<ParticipationTrackerTypes> = ({
  totalVotes,
  participation,
  availableVotes,
  hideProgressBar = false,
  className,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={cx(className, styles.tracker)}>
      <div className={styles.tracker__info}>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.totalVotesValue", {
            value: totalVotes,
          })}
          <span>{t("proposals.participationTracker.totalVotes")}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.participationValue", {
            value: participation,
          })}
          <span>{t("proposals.participationTracker.participation")}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.votesAvailableValue", {
            value: availableVotes,
          })}
          <span>{t("proposals.participationTracker.votesAvailable")}</span>
        </div>
      </div>
      <div
        className={cx(styles.tracker__bar, {
          [styles.hidden]: hideProgressBar,
        })}
      >
        <div style={{ width: `${participation}%` }} />
      </div>
    </div>
  );
};

export default ParticipationTracker;
