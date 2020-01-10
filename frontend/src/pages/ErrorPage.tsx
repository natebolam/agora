import React from "react";
import { FunctionComponent, ReactElement } from "react";
import styles from "~/styles/pages/ErrorPage.scss";
import { Link } from "react-navi";
import Helmet from "react-helmet";
import { useTranslation } from "react-i18next";
import Logo from "~/assets/svg/Logo";
import { NotFoundBoundary } from "react-navi";

const ErrorPage: FunctionComponent = ({ children }): ReactElement => {
  const { t } = useTranslation();
  const errorCode: string = "404";

  return (
    <NotFoundBoundary
      render={() => (
        <div className={styles.errorPage__wrapper}>
          <Helmet bodyAttributes={{ style: "background-color : #fff" }} />

          <div className={styles.errorPage}>
            <div className={styles.errorPage__logo}>
              <Link href="/" className={styles.errorPage__logo__content}>
                <Logo />
              </Link>
            </div>
            <main className={styles.errorPage__content}>
              <div>
                <div className={styles.errorPage__content__caption}>
                  {t("errorPage.errorCodeCaption")}
                </div>
                <div className={styles.errorPage__content__code}>
                  {errorCode}
                </div>
              </div>
            </main>
            <footer className={styles.errorPage__footer}>
              <div className={styles.errorPage__footer__content}>
                {errorCode === "404" ? (
                  <>
                    <p className={styles.header}>
                      {t(`errorPage.errors.${errorCode}.errorCaption`)}
                    </p>
                    <p className={styles.description}>
                      {t(`errorPage.errors.${errorCode}.errorDescription`)}
                    </p>
                  </>
                ) : null}
                <Link href="/stage">{t("errorPage.homeButtonCaption")}</Link>
              </div>
            </footer>
          </div>
        </div>
      )}
    >
      {children}
    </NotFoundBoundary>
  );
};

export default ErrorPage;
