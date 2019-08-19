import React, { FunctionComponent, ReactElement } from "react";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/ProposalStagePage.scss";
import { ProposalPeriodInfo } from "~/models/Period";
import ProposalPieChart from "~/components/proposals/graphs/ProposalPieChart";
import ProposalsList from "~/components/proposals/ProposalsList";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import RecentVotes from "~/components/proposals/RecentVotes";
import { useSelector } from "react-redux";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import { ProposalsList as ProposalsListType } from "~/models/ProposalsList";
import { RootStoreType } from "~/store";

interface ProposalViewProps {
  period: ProposalPeriodInfo;
}

const ProposalView: FunctionComponent<ProposalViewProps> = ({
  period,
}): ReactElement => {
  const proposalVotes: ProposalVotesListItem[] = useSelector(
    ({ periodStore }: RootStoreType): ProposalVotesListItem[] => {
      return periodStore.proposalVotes ? periodStore.proposalVotes.data : [];
    }
  );

  const proposals: ProposalsListType | null = useSelector(
    ({ periodStore }: RootStoreType): ProposalsListType | null => {
      if (!periodStore.proposalsLoading && periodStore.proposals)
        return periodStore.proposals;
      return null;
    }
  );

  return (
    <>
      <LayoutContent className={styles.period__primaryInfo}>
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
                voteStats={period.voteStats}
              />
            </div>
          </div>
        </div>
      </LayoutContent>
      <LayoutContent className={styles.period__secondaryInfo}>
        {proposals ? (
          <ProposalsList
            className={styles.proposal__info__proposalList}
            proposals={proposals}
            votesAvailable={period.voteStats.votesAvailable}
          />
        ) : null}
      </LayoutContent>
    </>
  );
};

export default ProposalView;
