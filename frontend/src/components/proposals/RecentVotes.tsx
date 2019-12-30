import React, { FunctionComponent, ReactElement, useState } from "react";
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
  const fullName = value.author;
  const [name, setName] = useState(value.author.substring(0, 15) + "...");

  const propHasTitle = !!value.proposalTitle;
  const proposalName = propHasTitle ? value.proposalTitle : value.proposal;

  return (
    <div className={styles.recentVotes__item}>
      <div className={styles.recentVotes__item__main}>
        <div
          className={styles.recentVotes__item__author}
          onClick={(): void => setName(fullName)}
        >
          {name}
        </div>
      </div>
      <div className={styles.recentVotes__item__operation}>
        <span>{t("proposals.recentVotes.proposalHashCaption")}</span>
        {proposalName}
      </div>
    </div>
  );
};

interface RecentVotesTypes {
  votes: ProposalVotesListItem[];
}

const RecentVotes: FunctionComponent<RecentVotesTypes> = ({
  votes,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <Card header={t("proposals.recentVotes.header.recentVotesCaption")}>
      {votes.map(
        (item: ProposalVotesListItem, index): ReactElement => (
          <RecentVotesItem value={item} key={index} />
        )
      )}
    </Card>
  );
};

export default RecentVotes;
