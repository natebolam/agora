import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/LearnMoreButton.scss";
import { useTranslation } from "react-i18next";

interface LearnMoreButtonTypes {
  className?: string;
  href: string;
}

const LearnMoreButton: FunctionComponent<LearnMoreButtonTypes> = ({
  className,
  href,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <a
      className={cx(className, styles.button)}
      href={href}
      target="_blank"
      rel="noopener noreferrer"
    >
      {t("common.learnMoreButton")}
    </a>
  );
};

export default LearnMoreButton;
