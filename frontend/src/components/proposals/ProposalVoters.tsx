import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import styles from "~/styles/components/proposals/ProposalVoters.scss";
import MajorityGraph from "~/components/proposals/graphs/MajorityGraph";
import { BallotsStats, VoteStats } from "~/models/Period";

interface ExplorationVotersTypes {
  className?: string;
  ballotsStats: BallotsStats;
  voteStats: VoteStats;
}

const ProposalVoters: FunctionComponent<ExplorationVotersTypes> = ({
  className,
  ballotsStats,
  voteStats,
}): ReactElement => {
  const participation: number = parseFloat(
    ((voteStats.votesCast / voteStats.votesAvailable) * 100).toFixed(0)
  );

  return (
    <div className={cx(className, styles.explorationVoters)}>
      <MajorityGraph ballotsStats={ballotsStats} voteStats={voteStats} />
      <ParticipationTracker
        className={styles.explorationVoters__tracker}
        availableVotes={voteStats.votesAvailable}
        participation={participation}
        totalVotes={voteStats.votesCast}
        hideProgressBar
      />
    </div>
  );
};

export default ProposalVoters;
