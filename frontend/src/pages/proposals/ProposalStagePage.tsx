import React, { FunctionComponent } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import ProposalPieChart from "~/components/proposals/ProposalPieChart.tsx";
import ProposalsList from "~/components/proposals/ProposalsList";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import BakersTable from "~/components/proposals/BakersTable";
import styles from "~/styles/pages/proposals/ProposalStagePage.scss";

const ProposalStagePage: FunctionComponent = () => {
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

  const participation = {
    totalVotes: 30000,
    participation: 10,
    availableVotes: 40000,
  };

  return (
    <Layout>
      <LayoutContent>
        <AgoraHeader />
        <PeriodHeader currentStage="proposal"/>
        <div className={styles.proposal__info}>
          <ProposalPieChart className={styles.proposal__info__chart}/>
          <div className={styles.proposal__info__general}>
            <ProposalsList className={styles.proposal__info__proposalList} proposals={proposals}/>
            <ParticipationTracker
              className={styles.proposal__info__votersInfo}
              totalVotes={participation.totalVotes}
              participation={participation.participation}
              availableVotes={participation.availableVotes}
            />
          </div>
        </div>
      </LayoutContent>
      <div className={styles.bakers__background}>
        <LayoutContent>
          <BakersTable/>
          <button className={styles.bakers__showMoreButton}>Show More</button>
        </LayoutContent>
      </div>
    </Layout>
  );
};

export default ProposalStagePage;
