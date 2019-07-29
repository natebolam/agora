import React, { FunctionComponent, ReactElement } from "react";
import Card from "~/components/common/Card";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import styles from "~/styles/components/proposals/RecentVoters.scss";
import { useTranslation } from "react-i18next";

interface RecentVotesItemTypes {
  value: ProposalVotesListItem;
}

const RecentVotesItem: FunctionComponent<RecentVotesItemTypes> = ({
  value,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={styles.recentVotes__item}>
      <div className={styles.recentVotes__item__main}>
        <div className={styles.recentVotes__item__author}>
          {value.author.name ? value.author.name : value.author.pkh}
        </div>
        <div className={styles.recentVotes__item__rolls}>
          {value.author.rolls}
        </div>
      </div>
      <div className={styles.recentVotes__item__operation}>
        <span>{t("proposals.recentVotes.proposalHashCaption")}</span>
        {value.operation}
      </div>
    </div>
  );
};

interface RecentVotesTypes {
  votes: ProposalVotesListItem[];
}

const RecentVotesHeader: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={styles.recentVotes__header}>
      <div>{t("proposals.recentVotes.header.recentVotesCaption")}</div>
      <div>{t("proposals.recentVotes.header.votesCaption")}</div>
    </div>
  );
};

const RecentVotes: FunctionComponent<RecentVotesTypes> = ({
  votes,
}): ReactElement => {
  return (
    <Card header={<RecentVotesHeader />}>
      {votes.map(
        (item: ProposalVotesListItem, index): ReactElement => (
          <RecentVotesItem value={item} key={index} />
        )
      )}
    </Card>
  );
};

export default RecentVotes;
