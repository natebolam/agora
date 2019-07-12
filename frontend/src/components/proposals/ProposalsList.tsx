import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import Card from "~/components/common/Card";
import * as styles from "~/styles/components/proposals/ProposalsList.scss";
import { useTranslation } from "react-i18next";

const ProposalListHeader: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={styles.list__header}>
      <div className={styles.list__header__title}>
        {t("proposals.proposalsList.proposalsHeaderCaption")}
      </div>
      <div className={styles.list__header__upvotes}>
        {t("proposals.proposalsList.upvotesHeaderCaption")}
      </div>
    </div>
  );
};

interface ProposalsListItemTypes {
  title: string;
  hash: string;
  upvotes: number;
}

const ProposalListItem: FunctionComponent<ProposalsListItemTypes> = ({
  title,
  hash,
  upvotes,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={styles.list__item}>
      <div className={styles.list__item__info}>
        <div className={styles.list__item__title}>{title}</div>
        <div className={styles.list__item__upvotes}>
          {upvotes.toLocaleString()}
        </div>
      </div>
      <div className={styles.list__item__hash}>
        <span>{t("proposals.proposalsList.hashCaption")}</span>
        {hash}
      </div>
    </div>
  );
};

interface ProposalsListTypes {
  className?: string;
  proposals: ProposalsListItemTypes[];
}

const ProposalsList: FunctionComponent<ProposalsListTypes> = ({
  className,
  proposals,
}): ReactElement => {
  return (
    <Card
      className={cx(className, styles.list)}
      header={<ProposalListHeader />}
    >
      {proposals.map(
        (proposal, index): ReactElement => (
          <ProposalListItem
            title={proposal.title}
            hash={proposal.hash}
            upvotes={proposal.upvotes}
            key={index}
          />
        )
      )}
    </Card>
  );
};

export default ProposalsList;
