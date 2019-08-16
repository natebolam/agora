import React, { FunctionComponent, ReactElement, useEffect } from "react";
import cx from "classnames";
import { ProposalTimeCircles } from "~/components/proposals/ProposalTimeTracker";
import { ButtonLink } from "~/components/common/ButtonLink";
import styles from "~/styles/pages/WelcomePage.scss";
import { useTranslation } from "react-i18next";
import { useDispatch, useSelector } from "react-redux";
import PeriodStore from "~/store/actions/periodActions";
import { PeriodType } from "~/models/Period";
import { RootStoreType } from "~/store";
import Logo from "~/assets/svg/Logo";
import useRouter from "use-react-router";

const WelcomePageHeader: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();

  return (
    <div className={styles.welcomePage__header}>
      <Logo />
      <div className={styles.welcomePage__header__caption}>
        {t("header.logoCaption")}
      </div>
    </div>
  );
};

interface AgoraLinksTypes {
  discourseLink?: string;
}

const AgoraLinks: FunctionComponent<AgoraLinksTypes> = ({
  discourseLink,
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <div className={styles.welcomePage__links}>
      <a href={t("tezosLinks.tezosWikiLink")}>
        <div className={styles.welcomePage__links__header}>
          {t("welcome.links.learnHeader")}
        </div>
        <div className={styles.welcomePage__links__content}>
          {t("welcome.links.learnDescription")}
        </div>
      </a>
      <a href={discourseLink}>
        <div className={styles.welcomePage__links__header}>
          {t("welcome.links.discussHeader")}
        </div>
        <div className={styles.welcomePage__links__content}>
          {t("welcome.links.discussDescription")}
        </div>
      </a>
      <a href={t("tezosLinks.getStartedLink")}>
        <div className={styles.welcomePage__links__header}>
          {t("welcome.links.getStartedHeader")}
        </div>
        <div className={styles.welcomePage__links__content}>
          {t("welcome.links.getStartedDescription")}
        </div>
      </a>
    </div>
  );
};

interface CurrentPeriodInfoTypes {
  className?: string;
  currentPeriodId?: number;
  currentCycle?: number;
  periodType?: PeriodType;
  timeRemaining?: string;
}

const CurrentPeriodInfo: FunctionComponent<CurrentPeriodInfoTypes> = ({
  className,
  currentPeriodId,
  currentCycle = 0,
  periodType,
  timeRemaining,
}): ReactElement => {
  const { t } = useTranslation();

  const currentPeriodCaption = t(`periodType.${periodType}`);

  return (
    <div className={cx(className, styles.welcomePage__period)}>
      <div className={styles.welcomePage__period__header}>
        {t("welcome.currentPeriod.header")}
      </div>
      <ButtonLink
        className={styles.welcomePage__period__button}
        to={`/period/${currentPeriodId}`}
      >
        {currentPeriodCaption}
      </ButtonLink>
      <ProposalTimeCircles
        className={styles.welcomePage__period__timeTracker}
        total={8}
        filled={currentCycle}
        circleSize={30}
        borderSize={4}
      />
      <div className={styles.welcomePage__period__remaining}>
        {t("welcome.currentPeriod.remainingTime", {
          value: {
            date: timeRemaining,
            options: {
              largest: 1,
            },
          },
        })}
      </div>
    </div>
  );
};

const WelcomePage: FunctionComponent = (): ReactElement => {
  const dispatch = useDispatch();
  const { history } = useRouter();

  useEffect((): void => {
    dispatch(PeriodStore.actionCreators.fetchWelcomePage());
  }, []);

  interface PeriodInfoTypes {
    loading: boolean;
    periodType?: PeriodType;
    endTime?: string;
    currentPeriodId?: number;
    currentCycle?: number;
    discourseLink?: string;
  }

  const periodInfo: PeriodInfoTypes = useSelector(
    (state: RootStoreType): PeriodInfoTypes => {
      return {
        loading: state.periodStore.loading,
        periodType: state.periodStore.period
          ? state.periodStore.period.type
          : undefined,
        endTime: state.periodStore.period
          ? state.periodStore.period.period.endTime
          : undefined,
        currentPeriodId: state.periodStore.period
          ? state.periodStore.period.period.id
          : undefined,
        currentCycle: state.periodStore.period
          ? state.periodStore.period.period.cycle
          : undefined,
        discourseLink: state.periodStore.period
          ? state.periodStore.period.discourseLink
          : undefined,
      };
    }
  );

  const errorCode: number | null = useSelector((state: RootStoreType):
    | number
    | null => {
    return state.periodStore.error ? state.periodStore.error.errorCode : null;
  });

  useEffect((): void => {
    console.log(errorCode);
    if (errorCode) {
      history.replace(`/error/${errorCode}`);
    }
  });

  return (
    <div className={styles.welcomePage_wrapper}>
      <div className={styles.welcomePage}>
        <WelcomePageHeader />
        {!periodInfo.loading && (
          <div className={styles.welcomePage__content}>
            <AgoraLinks discourseLink={periodInfo.discourseLink} />
            <CurrentPeriodInfo
              currentPeriodId={periodInfo.currentPeriodId}
              periodType={periodInfo.periodType}
              timeRemaining={periodInfo.endTime}
              currentCycle={periodInfo.currentCycle}
            />
          </div>
        )}
      </div>
    </div>
  );
};

export default WelcomePage;
