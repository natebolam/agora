import React, { FunctionComponent, ReactElement } from "react";
import { LineChart } from "recharts";

interface ProposalVotesGraphTypes {
  className?: string;
  votes: number[];
}

const ProposalVotesGraph: FunctionComponent<ProposalVotesGraphTypes> = ({
  className,
  votes,
}): ReactElement => {
  const objectVotes = votes.map((vote): { value: number } => ({
    value: vote,
  }));

  return (
    <LineChart className={className} width={70} height={20} data={objectVotes}>
      {/* TODO Get data from backend */}
      {/*<Line type="monotone" dataKey="value" stroke="#8884d8" dot={false} />*/}
    </LineChart>
  );
};

export default ProposalVotesGraph;
