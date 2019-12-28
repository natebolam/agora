import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import styles from "~/styles/components/common/Header.scss";
import { useTranslation } from "react-i18next";
import { Link } from "react-navi";
import MenuIcon from "~/assets/svg/MenuIcon";
import CloseIcon from "~/assets/svg/CloseIcon";
import Logo from "~/assets/svg/Logo";

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
        <Link href="/">
          <Logo />
          {t("header.logoCaption")}
        </Link>
      </div>
      <div className={styles.header__menu__links}>
        <a href={t("tezosLinks.tezosWikiLink")}>{t("header.wikiLink")}</a>
        <a href={t("tezosLinks.getStartedLink")}>
          {t("header.getStartedLink")}
        </a>
        <Link href={t("tezosLinks.tezosGovernanceLink")}>
          {t("header.governanceLink")}
        </Link>
      </div>
    </div>
  );
};

interface Props {
  className?: string;
}

const AgoraHeader: FunctionComponent<Props> = ({ className }): ReactElement => {
  const { t } = useTranslation();
  const [isMenuOpen, setMenuOpen] = useState(false);
  return (
    <div className={cx(className, styles.header)}>
      <div className={styles.header__logo}>
        <Link href="/stage">
          <Logo />
          {t("header.logoCaption")}
        </Link>
      </div>
      <div className={styles.header__links}>
        <a href={t("tezosLinks.tezosWikiLink")}>{t("header.wikiLink")}</a>
        <a href={t("tezosLinks.getStartedLink")}>
          {t("header.getStartedLink")}
        </a>
        <Link href={t("tezosLinks.tezosGovernanceLink")}>
          {t("header.governanceLink")}
        </Link>
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
