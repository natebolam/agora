import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import styles from "~/styles/components/proposals/ProposalVoters.scss";
import MajorityGraph from "~/components/proposals/graphs/MajorityGraph";
import { BallotsStats, VoteStats } from "~/models/Stage";

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
  return (
    <div className={cx(className, styles.explorationVoters)}>
      <MajorityGraph ballotsStats={ballotsStats} voteStats={voteStats} />
      <ParticipationTracker
        className={styles.explorationVoters__tracker}
        voteStats={voteStats}
        hideProgressBar
      />
    </div>
  );
};

export default ProposalVoters;
