import React, { FunctionComponent, ReactElement } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import { useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import {
  ExplorationPeriodInfo,
  MetaPeriodInfo,
  PromotionPeriodInfo,
  ProposalPeriodInfo,
  TestingPeriodInfo,
} from "~/models/Period";
import ProposalView from "~/pages/proposals/views/ProposalView";
import PromotionView from "~/pages/proposals/views/PromotionView";
import TestingView from "~/pages/proposals/views/TestingView";
import ExplorationView from "~/pages/proposals/views/ExplorationView";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import styles from "~/styles/pages/proposals/PeriodPage.scss";
import NoProposalView from "~/pages/proposals/views/NoProposalView";
import BusyIndicator from "react-busy-indicator";
import { useLoadingRoute } from "react-navi";

const PeriodPage: FunctionComponent = (): ReactElement => {
  const period: MetaPeriodInfo | null = useSelector(
    (state: RootStoreType): MetaPeriodInfo | null => {
      return state.periodStore.period;
    }
  );

  const hasProposal =
    period &&
    !(
      period.type === "proposal" &&
      (period as ProposalPeriodInfo).voteStats.votesCast === 0
    );
  const loadingRoute = useLoadingRoute();
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
      <LayoutContent className={styles.periodPage__header}>
        <AgoraHeader />
        {period && (
          <PeriodHeader
            currentStage={period.type}
            period={period.period}
            totalPeriods={period.totalPeriods}
            periodTimes={period.periodTimes}
          />
        )}
      </LayoutContent>
      {period && !hasProposal ? <NoProposalView /> : null}
      {period && hasProposal && period.type === "proposal" ? (
        <ProposalView period={period as ProposalPeriodInfo} />
      ) : null}
      {period && period.type === "exploration" ? (
        <ExplorationView period={period as ExplorationPeriodInfo} />
      ) : null}
      {period && period.type === "testing" ? (
        <TestingView period={period as TestingPeriodInfo} />
      ) : null}
      {period && period.type === "promotion" ? (
        <PromotionView period={period as PromotionPeriodInfo} />
      ) : null}
    </Layout>
  );
};

export default PeriodPage;
