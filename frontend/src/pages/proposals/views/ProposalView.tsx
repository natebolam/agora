import React, { FunctionComponent, ReactElement } from "react";
import { LayoutContent } from "~/components/common/Layout";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import styles from "~/styles/pages/proposals/PromotionStagePage.scss";
import { ProposalPeriodInfo } from "~/models/Period";
import ProposalPieChart from "~/components/proposals/ProposalPieChart";
import ProposalsList from "~/components/proposals/ProposalsList";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";

interface ProposalViewProps {
  period: ProposalPeriodInfo;
}

const ProposalView: FunctionComponent<ProposalViewProps> = ({
  period,
}): ReactElement => {
  const proposals = [
    {
      title: "Brasilia A",
      hash: "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd",
      upvotes: 10000,
    },
    {
      title: "Brasilia B",
      hash: "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd",
      upvotes: 5000,
    },
    {
      title: "Brasilia C",
      hash: "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd",
      upvotes: 100,
    },
  ];

  const { voteStats } = period;
  const availableVotes = parseFloat(
    ((voteStats.votesCast / voteStats.votesAvailable) * 100).toFixed(0)
  );

  return (
    <>
      <LayoutContent>
        <PeriodHeader
          currentStage="proposal"
          period={period.period}
          totalPeriods={period.totalPeriods}
        />
        <div className={styles.proposal__info}>
          <ProposalPieChart className={styles.proposal__info__chart} />
          <div className={styles.proposal__info__general}>
            <ProposalsList
              className={styles.proposal__info__proposalList}
              proposals={proposals}
            />
            <ParticipationTracker
              className={styles.proposal__info__votersInfo}
              totalVotes={period.voteStats.votesCast}
              participation={availableVotes}
              availableVotes={period.voteStats.votesAvailable}
            />
          </div>
        </div>
      </LayoutContent>
    </>
  );
};

export default ProposalView;
