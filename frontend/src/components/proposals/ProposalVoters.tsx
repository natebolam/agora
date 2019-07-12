import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import ChartMock from "~/assets/png/voters_mock.png";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import styles from "~/styles/components/proposals/ProposalVoters.scss";

interface ExplorationVotersTypes {
  className?: string;
}

const ProposalVoters: FunctionComponent<ExplorationVotersTypes> = ({
  className,
}): ReactElement => {
  const participation = {
    totalVotes: 30000,
    participation: 10,
    availableVotes: 40000,
  };
  return (
    <div className={cx(className, styles.explorationVoters)}>
      <img alt="" src={ChartMock} className={styles.explorationVoters__chart} />
      <ParticipationTracker
        className={styles.explorationVoters__tracker}
        availableVotes={participation.availableVotes}
        participation={participation.participation}
        totalVotes={participation.totalVotes}
        hideProgressBar
      />
    </div>
  );
};

export default ProposalVoters;
