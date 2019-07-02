import React, { FunctionComponent } from "react";
import cx from "classnames";
import Card from "~/components/common/Card";
import styles from "~/styles/components/proposals/ProposalsList.scss";
import { useTranslation } from "react-i18next";

const ProposalListHeader: FunctionComponent = () => {
  const { t } = useTranslation();
  return (
    <div className={styles.list__header}>
      <div className={styles.list__header__title}>
        {t('proposals.proposals_list.proposals_header_caption')}
      </div>
      <div className={styles.list__header__upvotes}>
        {t('proposals.proposals_list.upvotes_header_caption')}
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
  const { t } = useTranslation();
  return (
    <div className={styles.list__item}>
      <div className={styles.list__item__info}>
        <div className={styles.list__item__title}>
          {title}
        </div>
        <div className={styles.list__item__upvotes}>
          {upvotes.toLocaleString()}
        </div>
      </div>
      <div className={styles.list__item__hash}>
        <span>
        {t('proposals.proposals_list.hash_caption')}:
        </span>
        &nbsp;{hash}
      </div>
    </div>
  )
};

interface ProposalsListTypes {
  className?: string;
  proposals: ProposalsListItemTypes[]
}

const ProposalsList: FunctionComponent<ProposalsListTypes>
  = ({className, proposals}) => {
  return (
    <Card className={cx(className, styles.list)} header={<ProposalListHeader/>}>
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
