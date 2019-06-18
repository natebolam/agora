import React, { FunctionComponent } from "react";
import Card from "~/components/common/Card";
import styles from "~/styles/components/proposals/ProposalsList.scss";

const ProposalListHeader: FunctionComponent = () => {
  return (
    <div className={styles.list__header}>
      <div className={styles.list__header__title}>
        Proposals
      </div>
      <div className={styles.list__header__upvotes}>
        Upvotes
      </div>
    </div>
  )
};

interface ProposalsListItemTypes {
  title: string;
  hash: string;
  upvotes: number;
}

const ProposalListItem: FunctionComponent<ProposalsListItemTypes>
  = ({title, hash, upvotes}) => {
  return (
    <div className={styles.list__item}>
      <div className={styles.list__item__info}>
        {title}
        <div>
          <span>Hash:</span> {hash}
        </div>
      </div>
      <div className={styles.list__item__upvotes}>
        {upvotes.toLocaleString()}
      </div>
    </div>
  )
};

interface ProposalsListTypes {
  proposals: ProposalsListItemTypes[]
}

const ProposalsList: FunctionComponent<ProposalsListTypes>
  = ({proposals}) => {
  return (
    <Card header={<ProposalListHeader/>}>
        {
          proposals.map((proposal, index) => (
            <ProposalListItem
              title={proposal.title}
              hash={proposal.hash}
              upvotes={proposal.upvotes}
              key={index}
            />
          ))
        }
    </Card>
  )
};

export default ProposalsList;