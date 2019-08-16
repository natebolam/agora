import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";

interface VotesTableItemTypes {
  item: ProposalVotesListItem;
}

const VotesTableItem: FunctionComponent<VotesTableItemTypes> = ({
  item,
}): ReactElement => {
  const { t } = useTranslation();

  const millisecondsDuration =
    DateTime.local()
      .diff(DateTime.fromISO(item.timestamp))
      .shiftTo("minutes", "seconds")
      .get("minutes") *
    60 *
    1000;

  return (
    <tr>
      <td className={styles.name}>
        {item.author.name ? item.author.name : item.author.pkh}
      </td>
      <td className={styles.rolls}>{item.author.rolls}</td>
      <td className={styles.operation}>{item.operation}</td>
      <td className={styles.date}>
        {t("proposals.bakersTable.timeAgo", {
          value: {
            date: item.timestamp,
            milliseconds: millisecondsDuration,
            format: "dd MMM",
            withYearFormat: "dd MM yyyy",
            options: {
              largest: 1,
            },
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
  data,
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <table className={cx(className, styles.bakers)}>
      <thead>
        <tr>
          <th className={styles.name}>
            {t("proposals.bakersTable.header.baker")}
          </th>
          <th className={styles.rolls}>
            {t("proposals.bakersTable.header.votesAmount")}
          </th>
          <th className={styles.operation}>
            {t("proposals.bakersTable.header.hash")}
          </th>
          <th className={styles.date}>
            {t("proposals.bakersTable.header.date")}
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
