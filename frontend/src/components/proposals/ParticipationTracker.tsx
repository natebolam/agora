import React, {
  FunctionComponent,
  ReactElement,
  useEffect,
  createRef,
  RefObject,
} from "react";
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
  const trackerRef: RefObject<HTMLDivElement> = createRef();
  const connectorRef: RefObject<HTMLDivElement> = createRef();

  useEffect((): (() => void) => {
    const handleResize = (): void => {
      const tracker = trackerRef.current as HTMLElement;
      const connector = connectorRef.current as HTMLElement;

      const fstSpan = tracker.children[1].querySelector("span") as HTMLElement;
      const sndSpan = tracker.children[2].querySelector("span") as HTMLElement;

      const fstRect = fstSpan.getBoundingClientRect();
      const sndRect = sndSpan.getBoundingClientRect();

      const offset = fstSpan.offsetLeft + fstSpan.offsetWidth + 10;
      connector.style.left = `${offset}px`;
      connector.style.width = `${sndRect.left - fstRect.right - 20}px`;
    };

    handleResize();
    window.addEventListener("resize", handleResize);

    return (): void => window.removeEventListener("resize", handleResize);
  });

  return (
    <Card className={cx(className)} bodyClassName={styles.tracker__body}>
      <div className={styles.tracker__info} ref={trackerRef}>
        <div className={styles.tracker__info__connector} ref={connectorRef} />
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.participationValue", {
            value: participation,
          })}
          <span>{t("proposals.participationTracker.participation")}</span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.totalVotesValue", {
            value: voteStats.votesCast,
            available: voteStats.votesAvailable,
          })}
          <span>{t("proposals.participationTracker.totalVotes")}</span>
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
