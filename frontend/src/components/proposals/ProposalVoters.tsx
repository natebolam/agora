import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import ChartMock from "~/assets/png/voters_mock.png";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import styles from "~/styles/components/proposals/ProposalVoters.scss";

interface ExplorationVotersTypes {
  className?: string;
  votesCast: number;
  votesAvailable: number;
}

const ProposalVoters: FunctionComponent<ExplorationVotersTypes> = ({
  className,
  votesCast,
  votesAvailable,
}): ReactElement => {
  const participation: number = parseFloat(
    ((votesCast / votesAvailable) * 100).toFixed(0)
  );

  return (
    <div className={cx(className, styles.explorationVoters)}>
      <img alt="" src={ChartMock} className={styles.explorationVoters__chart} />
      <ParticipationTracker
        className={styles.explorationVoters__tracker}
        availableVotes={votesAvailable}
        participation={participation}
        totalVotes={votesCast}
        hideProgressBar
      />
    </div>
  );
};

export default ProposalVoters;
