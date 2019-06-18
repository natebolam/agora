import React, { FunctionComponent } from "react";
import Layout from "~/components/common/Layout";
import { AgoraHeader } from "~/components/common/Header";
import PeriodHeader from "~/components/proposals/PeriodHeader";

const ProposalPage: FunctionComponent = () => {
  return (
    <Layout>
      <AgoraHeader />
      <PeriodHeader />
    </Layout>
  );
};

export default ProposalPage;