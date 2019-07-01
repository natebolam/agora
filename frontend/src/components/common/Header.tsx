import React, { FunctionComponent } from "react";
import Logo from "~/assets/png/logo.png";
import styles from "~/styles/components/common/Header.scss";
import { useTranslation } from "react-i18next";

const AgoraHeader: FunctionComponent = () => {
  const { t } = useTranslation();

  return (
    <div className={styles.header}>
      <div className={styles.header__left}>
        <a href="#">
          <img src={Logo}/>
        </a>
        <a href="#" className={styles.header__logo}>
          {t('header.logo_caption')}
        </a>
      </div>
      <div className={styles.header__right}>
        <a href="#">{t('header.wiki_link_caption')}</a>
        <a href="#">{t('header.get_started_link_caption')}</a>
        <a href="#">{t('header.governance_link_caption')}</a>
      </div>
    </div>
  )
};

export default AgoraHeader;
