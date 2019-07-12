import React, { FunctionComponent, ReactElement } from "react";
import Logo from "~/assets/png/logo.png";
import styles from "~/styles/components/common/Header.scss";
import { useTranslation } from "react-i18next";

const AgoraHeader: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();

  return (
    <div className={styles.header}>
      <div className={styles.header__left}>
        <a href="#">
          <img alt="" src={Logo} />
        </a>
        <a href="#" className={styles.header__logo}>
          {t("header.logoCaption")}
        </a>
      </div>
      <div className={styles.header__right}>
        <a href="#">{t("header.wikiLink")}</a>
        <a href="#">{t("header.getStartedLink")}</a>
        <a href="#">{t("header.governanceLink")}</a>
      </div>
    </div>
  );
};

export default AgoraHeader;
