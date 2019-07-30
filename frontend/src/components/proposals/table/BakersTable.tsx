import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";
import { ProposalBallotsListItem } from "~/models/ProposalBallotsList";

interface BakersTableItemTypes {
  item: ProposalBallotsListItem;
}

const voteTypeCaption = (decision: "yay" | "nay" | "pass"): string => {
  const { t } = useTranslation();
  switch (decision) {
    case "yay":
      return t("proposals.bakersTable.voteYay");
    case "nay":
      return t("proposals.bakersTable.voteNay");
    case "pass":
      return t("proposals.bakersTable.votePass");
  }
};

const BakersTableItem: FunctionComponent<BakersTableItemTypes> = ({
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
      <td className={styles.decision}>{voteTypeCaption(item.decision)}</td>
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

interface BakersTableTypes {
  className?: string;
  data: ProposalBallotsListItem[];
}

const BakersTable: FunctionComponent<BakersTableTypes> = ({
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
          <th className={styles.decision}>
            {t("proposals.bakersTable.header.votesType")}
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
          (item: ProposalBallotsListItem, index: number): ReactElement => (
            <BakersTableItem key={index} item={item} />
          )
        )}
      </tbody>
    </table>
  );
};

export default BakersTable;
