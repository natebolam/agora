import React, { FunctionComponent, ReactElement } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import { useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import {
  MetaStageInfo,
  StageWithProposalInfo,
  VotingStageInfo,
  ImplementationStageInfo,
} from "~/models/Stage";
import VotingView from "~/pages/proposals/views/VotingView";
import ImplementationView from "~/pages/proposals/views/ImplementationView";
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
        <ProposalAndEvaluationView />
      ) : null}
      {stage && stage.type === "evaluation" ? (
        <ProposalAndEvaluationView />
      ) : null}
      {stage && stage.type === "voting" ? (
        <VotingView stage={stage as VotingStageInfo} />
      ) : null}
      {stage && stage.type === "implementation" ? (
        <ImplementationView stage={stage as ImplementationStageInfo} />
      ) : null}
    </Layout>
  );
};

export default StagePage;
