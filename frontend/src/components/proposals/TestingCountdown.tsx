import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/TestingCountdown.scss";
import { useTranslation } from "react-i18next";

interface TestingCountdownTypes {
  className?: string;
}

const TestingCountdown: FunctionComponent<TestingCountdownTypes>
  = ({ className }): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={cx(className, styles.countdown)}>
      <div className={styles.countdown__title}>
        {t('testing_period.countdown_caption')}
      </div>
      <div className={styles.countdown__datePeriod}>
        15 August, Sunday - 1 November, Monday
      </div>
      <div className={styles.countdown__timeLeft}>
        7 Days Remaining
      </div>
    </div>
  );
};

export default TestingCountdown;
