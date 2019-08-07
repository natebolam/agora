import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/DiscourseButton.scss";
import { useTranslation } from "react-i18next";
import ExternalIcon from "~/assets/svg/ExternalIcon";

interface DiscourseButtonTypes {
  className?: string;
  href: string;
}

const DiscourseButton: FunctionComponent<DiscourseButtonTypes> = ({
  className,
  href,
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <a className={cx(className, styles.button)} href={href}>
      {t("common.discourseButton")} <ExternalIcon />
    </a>
  );
};

export default DiscourseButton;
