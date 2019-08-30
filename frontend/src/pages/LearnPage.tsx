import React, { FunctionComponent, ReactElement } from "react";
import ReactMarkdown from "react-markdown";
import cx from "classnames";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import styles from "~/styles/pages/LearnPage.scss";
import Card from "~/components/common/Card";
import { useTranslation } from "react-i18next";
import BusyIndicator from "react-busy-indicator";
import { useLoadingRoute } from "react-navi";
import { useSelector } from "react-redux";
import { PeriodTimeInfo, Period, PeriodType } from "~/models/Period";
import { RootStoreType } from "~/store";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import { Proposal } from "~/models/ProposalInfo";

interface ToCItemTypes {
  item: HTMLHeadingElement;
}

const TocItem: FunctionComponent<ToCItemTypes> = ({ item }): ReactElement => {
  return (
    <div
      className={cx(
        styles.learnPage__toc_link,
        styles[`learnPage__toc_link_${item.tagName.toLowerCase()}`]
      )}
    >
      <a href={`#${item.id}`}>{item.textContent}</a>
    </div>
  );
};

interface LearnPageProps {
  source: string;
}

const LearnPage: FunctionComponent<LearnPageProps> = ({
  source,
}): ReactElement => {
  const { t } = useTranslation();
  const loadingRoute = useLoadingRoute();

  const period: Period | undefined = useSelector((state: RootStoreType):
    | Period
    | undefined => {
    return state.proposalStore.period;
  });

  const totalPeriods: number = useSelector((state: RootStoreType): number => {
    return state.proposalStore.totalPeriods;
  });

  const periodType: PeriodType | undefined = useSelector(
    (state: RootStoreType): PeriodType | undefined => {
      return state.proposalStore.periodType;
    }
  );
  const periodTimes: PeriodTimeInfo | undefined = useSelector(
    (state: RootStoreType): PeriodTimeInfo | undefined => {
      return state.proposalStore.periodTimes;
    }
  );

  const winner: Proposal | undefined = useSelector((state: RootStoreType):
    | Proposal
    | undefined => {
    return state.proposalStore.winner;
  });

  const temp = document.createElement("div");
  temp.innerHTML = source;

  const headings = Array.from(
    temp.querySelectorAll("h1, h2, h3, h4, h5, h6")
  ) as HTMLHeadingElement[];

  return (
    <Layout>
      <BusyIndicator
        active={!!loadingRoute}
        delayMs={0}
        className={""}
        color={"blue"}
        isBusy={!!loadingRoute}
        style={{}}
      />
      <LayoutContent className={styles.learnPage__header}>
        <AgoraHeader className={styles.learnPage__agoraHeader} />
        {period && periodTimes && periodType && (
          <PeriodHeader
            className={styles.learnPage__periodHeader}
            currentStage={periodType}
            period={period}
            totalPeriods={totalPeriods}
            periodTimes={periodTimes}
            proposal={winner || null}
            advanced={!!winner}
            isProposal={true}
          />
        )}
      </LayoutContent>
      <LayoutContent className={styles.learnPage__primaryInfo}>
        <h1 className={styles.learnPage__title}>{t("learnPage.title")}</h1>
        <div>
          {headings.map(
            (heading: HTMLHeadingElement, index: number): ReactElement => (
              <TocItem item={heading} key={index} />
            )
          )}
        </div>
      </LayoutContent>
      <LayoutContent>
        <Card className={styles.learnPage__card}>
          <ReactMarkdown source={source} escapeHtml={false} />
        </Card>
      </LayoutContent>
    </Layout>
  );
};

export default LearnPage;
