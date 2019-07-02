import React, { FunctionComponent, ReactElement } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import ProposalDescription from "~/components/proposals/ProposalDescription.tsx";
import styles from "~/styles/pages/proposals/ExplorationStagePage.scss";
import ProposalVoters from "~/components/proposals/ProposalVoters.tsx";
import BakersTable from "~/components/proposals/BakersTable";
import BakersFilter from "~/components/proposals/BakersFilter";

const ExplorationStagePage: FunctionComponent = (): ReactElement => {
  const proposalDescription = {
    title: "Brasilia",
    description: "New proposal reduces roll size and increases gas limit. " +
    "Proposer states in their announcement post that they " +
    "have performed extensive testing."
  };
  return (
    <Layout>
      <LayoutContent>
        <AgoraHeader />
        <PeriodHeader currentStage="exploration" />
        <div className={styles.exploration__info}>
          <ProposalDescription
            className={styles.exploration__description}
            title={proposalDescription.title}
            description={proposalDescription.description}
          />
          <ProposalVoters className={styles.exploration__voters} />
        </div>
      </LayoutContent>
      <div className={styles.bakers__background}>
        <LayoutContent>
          <BakersFilter className={styles.bakers__filter} />
          <BakersTable className={styles.bakers__table} />
          <button className={styles.bakers__showMoreButton}>Show More</button>
        </LayoutContent>
      </div>
    </Layout>
  );
};

export default ExplorationStagePage;
