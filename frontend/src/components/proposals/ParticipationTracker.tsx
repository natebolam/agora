import React, { FunctionComponent } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ParticipationTracker.scss";

interface ParticipationTrackerTypes {
  className?: string;
  totalVotes: number;
  participation: number;
  availableVotes: number;
}

const ParticipationTracker: FunctionComponent<ParticipationTrackerTypes>
  = ({totalVotes, participation, availableVotes, className}) => {
  return (
    <div className={cx(className, styles.tracker)}>
      <div className={styles.tracker__info}>
        <div className={styles.tracker__info__item}>
          {totalVotes.toLocaleString()}
          <span>Votes Cast</span>
        </div>
        <div className={styles.tracker__info__item}>
          {participation}%
          <span>Participation</span>
        </div>
        <div className={styles.tracker__info__item}>
          {availableVotes.toLocaleString()}
          <span>Votes Available</span>
        </div>
      </div>
      <div className={styles.tracker__bar}>
        <div style={{width: `${participation}%`}}/>
      </div>
    </div>
  )
};

export default ParticipationTracker;