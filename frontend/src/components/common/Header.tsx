import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import Logo from "~/assets/png/logo.png";
import styles from "~/styles/components/common/Header.scss";
import { useTranslation } from "react-i18next";
import { Link } from "react-router-dom";
import MenuIcon from "~/assets/svg/MenuIcon";
import CloseIcon from "~/assets/svg/CloseIcon";

interface HeaderMenuTypes {
  isOpen: boolean;
  onClose: () => void;
}

const HeaderMenu: FunctionComponent<HeaderMenuTypes> = ({
  isOpen,
  onClose,
}): ReactElement => {
  const body = document.getElementsByTagName("body").item(0);
  if (body) {
    body.setAttribute(
      "style",
      isOpen ? "position: fixed" : "position: initial"
    );
  }
  const { t } = useTranslation();
  return (
    <div
      className={cx(styles.header__menu, {
        [styles.header__menu_open]: isOpen,
      })}
    >
      <div className={styles.header__menu__closeButton} onClick={onClose}>
        <CloseIcon />
      </div>
      <div className={styles.header__logo}>
        <Link to="/">
          <img alt="" src={Logo} />
          {t("header.logoCaption")}
        </Link>
      </div>
      <div className={styles.header__menu__links}>
        <a href={t("tezosLinks.tezosWikiLink")}>{t("header.wikiLink")}</a>
        <a href={t("tezosLinks.getStartedLink")}>
          {t("header.getStartedLink")}
        </a>
        <a href={t("tezosLinks.tezosGovernanceLink")}>
          {t("header.governanceLink")}
        </a>
      </div>
    </div>
  );
};

const AgoraHeader: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();
  const [isMenuOpen, setMenuOpen] = useState(false);
  return (
    <div className={styles.header}>
      <div className={styles.header__logo}>
        <Link to="/">
          <img alt="" src={Logo} />
          {t("header.logoCaption")}
        </Link>
      </div>
      <div className={styles.header__links}>
        <a href={t("tezosLinks.tezosWikiLink")}>{t("header.wikiLink")}</a>
        <a href={t("tezosLinks.getStartedLink")}>
          {t("header.getStartedLink")}
        </a>
        <a href={t("tezosLinks.tezosGovernanceLink")}>
          {t("header.governanceLink")}
        </a>
      </div>
      <div
        className={styles.header__expandMenuButton}
        onClick={(): void => setMenuOpen(true)}
      >
        <MenuIcon />
      </div>
      <div>
        <HeaderMenu
          isOpen={isMenuOpen}
          onClose={(): void => setMenuOpen(false)}
        />
      </div>
    </div>
  );
};

export default AgoraHeader;
