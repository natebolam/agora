import React, { FunctionComponent, ReactElement } from "react";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/ProposalStagePage.scss";
import ProposalsList from "~/components/proposals/ProposalsList";
import { useSelector } from "react-redux";
import { ProposalsList as ProposalsListType } from "~/models/ProposalInfo";
import { RootStoreType } from "~/store";

const ProposalAndEvaluationView: FunctionComponent = (): ReactElement => {
  const proposals: ProposalsListType | null = useSelector(
    ({ stageStore }: RootStoreType): ProposalsListType | null => {
      if (!stageStore.proposalsLoading && stageStore.proposals)
        return stageStore.proposals;
      return null;
    }
  );

  return (
    <LayoutContent className={styles.stage__secondaryInfo}>
      {proposals ? (
        <ProposalsList
          className={styles.proposal__info__proposalList}
          proposals={proposals}
          isProposalorEvaluation={true}
        />
      ) : null}
    </LayoutContent>
  );
};

export default ProposalAndEvaluationView;
