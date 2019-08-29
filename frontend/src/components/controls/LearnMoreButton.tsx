import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/LearnMoreButton.scss";
import { useTranslation } from "react-i18next";
import { Link } from "react-navi";

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
    <Link className={cx(className, styles.button)} href={href}>
      {t("common.learnMoreButton")}
    </Link>
  );
};

export default LearnMoreButton;
