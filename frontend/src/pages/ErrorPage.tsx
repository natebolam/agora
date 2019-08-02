import React from "react";
import { FunctionComponent, ReactElement } from "react";
import styles from "~/styles/pages/ErrorPage.scss";
import { Link } from "react-router-dom";
import Helmet from "react-helmet";
import { useTranslation } from "react-i18next";
import useRouter from "use-react-router";
import Logo from "~/assets/svg/Logo";

const ErrorPage: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();
  const { match } = useRouter();
  const errorCodeMatch: RegExpMatchArray | null = match.path.match(
    /\/error\/(\d+)/
  );
  const errorCode: string = errorCodeMatch ? errorCodeMatch[1] : "404";

  return (
    <div className={styles.errorPage__wrapper}>
      <Helmet bodyAttributes={{ style: "background-color : #fff" }} />

      <div className={styles.errorPage}>
        <div className={styles.errorPage__logo}>
          <Link to="/" className={styles.errorPage__logo__content}>
            <Logo />
            <div>{t("header.logoCaption")}</div>
          </Link>
        </div>
        <main className={styles.errorPage__content}>
          <div>
            <div className={styles.errorPage__content__caption}>
              {t("errorPage.errorCodeCaption")}
            </div>
            <div className={styles.errorPage__content__code}>{errorCode}</div>
          </div>
        </main>
        <footer className={styles.errorPage__footer}>
          <div className={styles.errorPage__footer__content}>
            {errorCode === "404" || errorCode === "500" ? (
              <>
                <p className={styles.header}>
                  {t(`errorPage.errors.${errorCode}.errorCaption`)}
                </p>
                <p className={styles.description}>
                  {t(`errorPage.errors.${errorCode}.errorDescription`)}
                </p>
              </>
            ) : null}
            <Link to="/">{t("errorPage.homeButtonCaption")}</Link>
          </div>
        </footer>
      </div>
    </div>
  );
};

export default ErrorPage;
