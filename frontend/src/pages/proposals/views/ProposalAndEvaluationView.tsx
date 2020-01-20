import React, { FunctionComponent, ReactElement } from "react";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/ProposalStagePage.scss";
import ProposalsList from "~/components/proposals/ProposalsList";
import { ProposalsList as ProposalsListType } from "~/models/ProposalInfo";

interface ProposalAndEvaluationViewProps {
  proposals: ProposalsListType;
}

const ProposalAndEvaluationView: FunctionComponent<ProposalAndEvaluationViewProps> = ({
  proposals,
}): ReactElement => (
  <LayoutContent className={styles.stage__secondaryInfo}>
    <ProposalsList
      className={styles.proposal__info__proposalList}
      proposals={proposals.slice(0, 1)}
    />
  </LayoutContent>
);

export default ProposalAndEvaluationView;
