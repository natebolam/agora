import React, { FunctionComponent } from "react";
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

const ParticipationTracker: FunctionComponent<ParticipationTrackerTypes>
  = ({totalVotes, participation, availableVotes, hideProgressBar, className}) => {
  const { t } = useTranslation();
  return (
    <div className={cx(className, styles.tracker)}>
      <div className={styles.tracker__info}>
        <div className={styles.tracker__info__item}>
          {totalVotes.toLocaleString()}
          <span>{t('proposals.participation_tracker.total_votes')}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {participation}%
          <span>{t('proposals.participation_tracker.participation')}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {availableVotes.toLocaleString()}
          <span>{t('proposals.participation_tracker.votes_available')}</span>
        </div>
      </div>
      <div className={cx(styles.tracker__bar, {[styles.hidden]: hideProgressBar})}>
        <div style={{width: `${participation}%`}}/>
      </div>
    </div>
  )
};

ParticipationTracker.defaultProps = {
  hideProgressBar: false,
};

export default ParticipationTracker;
