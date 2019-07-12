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

  const secondsDuration = DateTime.fromISO(dateTo)
    .diffNow()
    .shiftTo("milliseconds")
    .get("milliseconds");

  return (
    <div className={cx(className, styles.countdown)}>
      <div className={styles.countdown__title}>
        {t("proposals.testingCountdown.countdownCaption")}
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
        {t("proposals.testingCountdown.remainingTime", {
          value: {
            milliseconds: secondsDuration,
            options: {
              largest: 1,
            },
          },
        })}
      </div>
    </div>
  );
};

export default TestingCountdown;
