import React, {
  FunctionComponent,
  ReactElement,
  useEffect,
  createRef,
  RefObject,
  useState,
} from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/ParticipationTracker.scss";
import { useTranslation } from "react-i18next";
import Card from "~/components/common/Card";
import { VoteStats } from "~/models/Period";

type InvertChangeHandler = (inverted: boolean) => void;

interface ParticipationTrackerTypes {
  className?: string;
  voteStats: VoteStats;
  hideProgressBar?: boolean;
  onInvert?: InvertChangeHandler;
}

const ParticipationTracker: FunctionComponent<ParticipationTrackerTypes> = ({
  voteStats,
  hideProgressBar = false,
  className,
  onInvert = (): void => {},
}): ReactElement => {
  const { t } = useTranslation();
  const trackerRef: RefObject<HTMLDivElement> = createRef();
  const connectorRef: RefObject<HTMLDivElement> = createRef();
  const [inverted, setInverted] = useState(false);

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

  const participation = (value: number): string =>
    ((value / voteStats.votesAvailable) * 100).toFixed(2);

  const convertData = (value: number, all: number): number =>
    inverted ? all - value : value;

  const invert = (): void => {
    const newInverted = !inverted;
    setInverted(newInverted);
    onInvert(newInverted);
  };

  return (
    <Card className={cx(className)} bodyClassName={styles.tracker__body}>
      <div className={styles.tracker__info} ref={trackerRef} onClick={invert}>
        <div className={styles.tracker__info__connector} ref={connectorRef} />
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.participationValue", {
            value: participation(
              convertData(voteStats.votesCast, voteStats.votesAvailable)
            ),
          })}
          <span>
            {t(
              "proposals.participationTracker.participation" +
                (inverted ? "Inverted" : "")
            )}
          </span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.totalVotesValue", {
            value: convertData(voteStats.votesCast, voteStats.votesAvailable),
            available: voteStats.votesAvailable,
          })}
          <span>
            {t(
              "proposals.participationTracker.totalVotes" +
                (inverted ? "Inverted" : "")
            )}
          </span>
        </div>
        <div className={styles.tracker__info__item}>
          {t("proposals.participationTracker.numVotersValue", {
            value: convertData(voteStats.numVoters, voteStats.numVotersTotal),
            total: voteStats.numVotersTotal,
          })}
          <span>
            {t(
              "proposals.participationTracker.numVoters" +
                (inverted ? "Inverted" : "")
            )}
          </span>
        </div>
      </div>
      <div
        className={cx(styles.tracker__bar, {
          [styles.hidden]: hideProgressBar,
        })}
      >
        <div
          style={{
            width: `${participation(
              convertData(voteStats.votesCast, voteStats.votesAvailable)
            )}%`,
          }}
        />
      </div>
    </Card>
  );
};

export default ParticipationTracker;
