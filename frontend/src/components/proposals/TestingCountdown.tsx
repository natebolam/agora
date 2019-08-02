import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/TestingCountdown.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";

interface TestingCountdownTypes {
  className?: string;
  dateFrom: string;
  dateTo: string;
}

const TestingCountdown: FunctionComponent<TestingCountdownTypes> = ({
  className,
  dateFrom,
  dateTo,
}): ReactElement => {
  const { t } = useTranslation();

  const finished = DateTime.fromISO(dateTo).diffNow().milliseconds < 0;
  const finishedRemainingTimeCaption = t(
    "proposals.testingCountdown.remainingTimeFinished"
  );
  const remainingTimeCpation = t("proposals.testingCountdown.remainingTime", {
    value: {
      date: dateTo,
      options: {
        largest: 1,
      },
    },
  });

  return (
    <div className={cx(className, styles.countdown)}>
      <div className={styles.countdown__title}>
        {finished
          ? t("proposals.testingCountdown.countdownFinishedCaption")
          : t("proposals.testingCountdown.countdownCaption")}
      </div>
      <div className={styles.countdown__datePeriod}>
        {t("proposals.testingCountdown.periodDate", {
          from: {
            date: dateFrom,
            format: "dd MMMM, cccc",
          },
          to: {
            date: dateTo,
            format: "dd MMMM, cccc",
          },
        })}
      </div>
      <div className={styles.countdown__timeLeft}>
        {finished ? finishedRemainingTimeCaption : remainingTimeCpation}
      </div>
    </div>
  );
};

export default TestingCountdown;
