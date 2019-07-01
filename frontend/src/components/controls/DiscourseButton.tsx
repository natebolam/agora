import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/DiscourseButton.scss";
import { useTranslation } from "react-i18next";
import ExternalIcon from "~/assets/png/external_icon.png";

interface DiscourseButtonTypes {
  className?: string;
}

const DiscourseButton: FunctionComponent<DiscourseButtonTypes> = ({
  className
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <a className={cx(className!, styles.button)} href="#">
      {t("common.discourse_button")} <img src={ExternalIcon} />
    </a>
  );
};

export default DiscourseButton;
