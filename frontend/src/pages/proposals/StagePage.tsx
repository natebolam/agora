import React, { FunctionComponent, ReactElement } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import { useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import { ProposalsList } from "~/models/ProposalInfo";
import {
  MetaStageInfo,
  VotingStageInfo,
  ImplementationStageInfo,
} from "~/models/Stage";
import NoProposalView from "~/pages/proposals/views/NoProposalView";
import VotingView from "~/pages/proposals/views/VotingView";
import ImplementationView from "~/pages/proposals/views/ImplementationView";
import StageHeader from "~/components/proposals/StageHeader";
import ProposalAndEvaluationView from "~/pages/proposals/views/ProposalAndEvaluationView";
import styles from "~/styles/pages/proposals/StagePage.scss";
import BusyIndicator from "react-busy-indicator";
import { useLoadingRoute } from "react-navi";

const StagePage: FunctionComponent = (): ReactElement => {
  const stage: MetaStageInfo | null = useSelector(
    ({ stageStore }: RootStoreType): MetaStageInfo | null => {
      return stageStore.stage;
    }
  );
  const proposals: ProposalsList = useSelector(
    ({ stageStore }: RootStoreType): ProposalsList =>
      !stageStore.proposalsLoading && stageStore.proposals
        ? stageStore.proposals
        : []
  );

  const loadingRoute = useLoadingRoute();

  const stageView = (stage: MetaStageInfo | null): ReactElement | null => {
    if (!stage) {
      return null;
    } else if (stage.type === "proposal") {
      return <ProposalAndEvaluationView proposals={proposals} />;
    } else if (stage.type === "evaluation") {
      return <ProposalAndEvaluationView proposals={proposals} />;
    } else if (stage.type === "voting") {
      return (
        <VotingView stage={stage as VotingStageInfo} proposal={proposals[0]} />
      );
    } else if (stage.type === "implementation") {
      return <ImplementationView stage={stage as ImplementationStageInfo} />;
    } else {
      return null;
    }
  };

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
          />
        )}
      </LayoutContent>

      {!stage || proposals.length > 0 ? (
        stageView(stage)
      ) : (
        <NoProposalView isProposalStage={stage.type === "proposal"} />
      )}
    </Layout>
  );
};

export default StagePage;
