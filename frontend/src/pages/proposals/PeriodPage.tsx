import React, { FunctionComponent, ReactElement, useEffect } from "react";
import useRouter from "use-react-router";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import { useDispatch, useSelector } from "react-redux";
import PeriodStore from "~/store/actions/periodActions";
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

interface PeriodRouterParams {
  id: number;
}

const PeriodPage: FunctionComponent = (): ReactElement => {
  const { match } = useRouter();
  const dispatch = useDispatch();

  const id = (match.params as PeriodRouterParams).id;

  useEffect((): void => {
    dispatch(PeriodStore.actionCreators.fetchPeriod(id));
  }, [id]);

  const period: MetaPeriodInfo | null = useSelector(
    (state: RootStoreType): MetaPeriodInfo | null => {
      return state.period.period;
    }
  );

  const loading: boolean = useSelector((state: RootStoreType): boolean => {
    return state.period.loading;
  });

  return (
    <Layout>
      <LayoutContent>
        <AgoraHeader />
      </LayoutContent>
      {!loading && period && period.type === "proposal" ? (
        <ProposalView period={period as ProposalPeriodInfo} />
      ) : null}
      {!loading && period && period.type === "exploration" ? (
        <ExplorationView period={period as ExplorationPeriodInfo} />
      ) : null}
      {!loading && period && period.type === "testing" ? (
        <TestingView period={period as TestingPeriodInfo} />
      ) : null}
      {!loading && period && period.type === "promotion" ? (
        <PromotionView period={period as PromotionPeriodInfo} />
      ) : null}
    </Layout>
  );
};

export default PeriodPage;
