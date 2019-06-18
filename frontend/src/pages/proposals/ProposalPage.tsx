import React, { FunctionComponent } from "react";
import Layout from "~/components/common/Layout";
import { AgoraHeader } from "~/components/common/Header";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import ProposalPieChart from "~/components/proposals/ProposalPieChart.tsx";

const ProposalPage: FunctionComponent = () => {
  return (
    <Layout>
      <AgoraHeader />
      <PeriodHeader />
      <div>
        <ProposalPieChart />
      </div>
    </Layout>
  );
};

export default ProposalPage;