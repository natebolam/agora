import React, { FunctionComponent } from "react";
import Layout from "~/components/common/Layout";
import { AgoraHeader } from "~/components/common/Header";
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
      <AgoraHeader />
      <PeriodHeader />
      <div style={{display: "flex", justifyContent: "space-between", padding: 0}}>
        <ProposalPieChart />
        <div>
          <ProposalsList proposals={proposals}/>
          <ParticipationTracker
            totalVotes={participation.totalVotes}
            participation={participation.participation}
            availableVotes={participation.availableVotes}
          />
        </div>
      </div>
      <div className={styles.bakers__background}>
        <BakersTable/>
        <button className={styles.bakers__showMoreButton}>Show More</button>
      </div>
    </Layout>
  );
};

export default ProposalStagePage;