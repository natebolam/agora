import React, { FunctionComponent } from "react";
import Layout from "~/components/common/Layout";
import { AgoraHeader } from "~/components/common/Header";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import ProposalPieChart from "~/components/proposals/ProposalPieChart.tsx";
import ProposalsList from "~/components/proposals/ProposalsList";

const ProposalPage: FunctionComponent = () => {
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

  return (
    <Layout>
      <AgoraHeader />
      <PeriodHeader />
      <div style={{display: "flex", justifyContent: "space-between"}}>
        <ProposalPieChart />
        <ProposalsList proposals={proposals}/>
      </div>
    </Layout>
  );
};

export default ProposalPage;