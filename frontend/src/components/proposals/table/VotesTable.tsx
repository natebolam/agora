import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/VotersTable.scss";
import { useTranslation } from "react-i18next";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";

interface VotesTableItemTypes {
  item: ProposalVotesListItem;
}

const VotesTableItem: FunctionComponent<VotesTableItemTypes> = ({
  item,
}): ReactElement => {
  const { t } = useTranslation();

  const name = (): JSX.Element | string => {
    const text = item.author;
    return <span>{text}</span>;
  };

  return (
    <tr>
      <td className={styles.name}>{name()}</td>
      <td
        className={styles.date}
        title={t("proposals.votersTable.time", {
          value: {
            date: item.timestamp,
            format: "hh:mm:ss dd MMM yyyy",
          },
        })}
      >
        {t("proposals.votersTable.timeAgo", {
          value: {
            date: item.timestamp,
            format: "DDD 'at' t",
            timeFormat: "t",
          },
        })}
      </td>
    </tr>
  );
};

interface VotesTableTypes {
  className?: string;
  data: ProposalVotesListItem[];
}

const VotesTable: FunctionComponent<VotesTableTypes> = ({
  className,
  data: initialData,
}): ReactElement => {
  const { t } = useTranslation();
  const data = [...initialData];

  return (
    <table className={cx(className, styles.votes)}>
      <thead>
        <tr>
          <th className={styles.name}>
            {t("proposals.votersTable.header.voter")}
          </th>
          <th className={styles.date}>
            {t("proposals.votersTable.header.time")}
          </th>
        </tr>
      </thead>
      <tbody>
        {data.map(
          (item: ProposalVotesListItem, index: number): ReactElement => (
            <VotesTableItem key={index} item={item} />
          )
        )}
      </tbody>
    </table>
  );
};

export default VotesTable;
