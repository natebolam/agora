import React, { FunctionComponent, ReactElement } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import { useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import {
  MetaStageInfo,
  ProposalStageInfo,
  StageWithProposalInfo,
  VotingStageInfo,
  ImplementationStageInfo,
  EvaluationStageInfo,
} from "~/models/Stage";
import ProposalView from "~/pages/proposals/views/ProposalView";
import TestingView from "~/pages/proposals/views/TestingView";
import StageHeader from "~/components/proposals/StageHeader";
import ProposalAndEvaluationView from "~/pages/proposals/views/ProposalAndEvaluationView";
import styles from "~/styles/pages/proposals/StagePage.scss";
import BusyIndicator from "react-busy-indicator";
import { useLoadingRoute } from "react-navi";

const StagePage: FunctionComponent = (): ReactElement => {
  const stage: MetaStageInfo | null = useSelector(
    (state: RootStoreType): MetaStageInfo | null => {
      return state.stageStore.stage;
    }
  );

  const proposal =
    stage &&
    (stage.type == "proposal"
      ? (stage as VotingStageInfo).winner
      : (stage as StageWithProposalInfo).proposal);

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
      <LayoutContent className={styles.stagePage__header}>
        <AgoraHeader />
        {stage && (
          <StageHeader
            currentStage={stage.type}
            stage={stage.stage}
            totalStages={stage.totalStages}
            stageTimes={stage.stageTimes}
            proposal={proposal}
          />
        )}
      </LayoutContent>
      {stage && stage.type === "proposal" ? (
        <ProposalAndEvaluationView stage={stage as ProposalStageInfo} />
      ) : null}
      {stage && stage.type === "evaluation" ? (
        <ProposalAndEvaluationView stage={stage as EvaluationStageInfo} />
      ) : null}
      {stage && stage.type === "voting" ? (
        <ProposalView stage={stage as VotingStageInfo} />
      ) : null}
      {stage && stage.type === "implementation" ? (
        <TestingView stage={stage as ImplementationStageInfo} />
      ) : null}
    </Layout>
  );
};

export default StagePage;
