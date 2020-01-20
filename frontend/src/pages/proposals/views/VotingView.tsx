import React, { FunctionComponent, ReactElement } from "react";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/VotingStagePage.scss";
import { VotingStageInfo } from "~/models/Stage";
import QuorumGraph from "~/components/proposals/graphs/QuorumGraph";
import VotesTable from "~/components/proposals/table/VotesTable";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import { useTranslation } from "react-i18next";
import { useSelector } from "react-redux";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import { Proposal } from "~/models/ProposalInfo";
import { RootStoreType } from "~/store";

interface VotingViewProps {
  stage: VotingStageInfo;
  proposal: Proposal;
}

const VotingView: FunctionComponent<VotingViewProps> = ({
  stage,
  proposal,
}): ReactElement => {
  const proposalVotes: ProposalVotesListItem[] = useSelector(
    ({ stageStore }: RootStoreType): ProposalVotesListItem[] => {
      return stageStore.proposalVotes ? stageStore.proposalVotes : [];
    }
  );

  const { t } = useTranslation();

  return (
    <>
      <LayoutContent className={styles.stage__primaryInfo}>
        <div>
          <ProposalDescription
            className={styles.voting__description}
            title={proposal.title ? proposal.title : proposal.hash}
            description={
              proposal.shortDescription
                ? proposal.shortDescription
                : t("proposals.common.noShortDescriptionCaption")
            }
            discourseLink={proposal.discourseLink}
            learnMoreLink={`/proposal/${proposal.stage}/${proposal.id}`}
          />
          <div className={styles.voting__voters}>
            <QuorumGraph
              maxValue={stage.voteStats.numVotersTotal}
              value={stage.voteStats.numVoters}
              quorumValue={stage.voteStats.numVotersTotal / 2}
              quorumMarkLabel="Quorum"
            />
            <ParticipationTracker voteStats={stage.voteStats} hideProgressBar />
          </div>
        </div>
      </LayoutContent>

      <LayoutContent className={styles.stage__secondaryInfo}>
        {proposalVotes && (
          <VotesTable data={proposalVotes} className={styles.voters__table} />
        )}
      </LayoutContent>
    </>
  );
};

export default VotingView;
