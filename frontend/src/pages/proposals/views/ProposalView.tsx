import React, { FunctionComponent, ReactElement } from "react";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/ProposalStagePage.scss";
import { VotingStageInfo } from "~/models/Stage";
import ProposalPieChart from "~/components/proposals/graphs/ProposalPieChart";
import ProposalsList from "~/components/proposals/ProposalsList";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import RecentVotes from "~/components/proposals/RecentVotes";
import { useSelector } from "react-redux";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import { ProposalsList as ProposalsListType } from "~/models/ProposalInfo";
import { RootStoreType } from "~/store";

interface VotingViewProps {
  stage: VotingStageInfo;
}

const ProposalView: FunctionComponent<VotingViewProps> = ({
  stage,
}): ReactElement => {
  const proposalVotes: ProposalVotesListItem[] = useSelector(
    ({ stageStore }: RootStoreType): ProposalVotesListItem[] => {
      return stageStore.proposalVotes ? stageStore.proposalVotes : [];
    }
  );

  const proposals: ProposalsListType | null = useSelector(
    ({ stageStore }: RootStoreType): ProposalsListType | null => {
      if (!stageStore.proposalsLoading && stageStore.proposals)
        return stageStore.proposals;
      return null;
    }
  );

  return (
    <>
      <LayoutContent className={styles.stage__primaryInfo}>
        <div>
          <div className={styles.left}>
            <ProposalPieChart className={styles.proposal__info__chart} />
          </div>
          <div className={styles.right}>
            <div className={styles.right__top}>
              <RecentVotes votes={proposalVotes} />
            </div>
            <div className={styles.right__bottom}>
              <ParticipationTracker
                className={styles.proposal__info__votersInfo}
                voteStats={stage.voteStats}
              />
            </div>
          </div>
        </div>
      </LayoutContent>
      <LayoutContent className={styles.stage__secondaryInfo}>
        {proposals ? (
          <ProposalsList
            className={styles.proposal__info__proposalList}
            proposals={proposals}
            isProposalorEvaluation={false}
            votesAvailable={stage.voteStats.numVotersTotal}
          />
        ) : null}
      </LayoutContent>
    </>
  );
};

export default ProposalView;
