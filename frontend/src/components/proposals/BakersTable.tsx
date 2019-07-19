import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";
import { ProposalBallotsListItem } from "~/models/ProposalBallotsList";

interface BakersTableItemTypes {
  name: string;
  upvoteTotal: number;
  decision: "yay" | "nay" | "pass";
  hash: string;
  date: string;
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
  name,
  upvoteTotal,
  decision,
  hash,
  date,
}): ReactElement => {
  const { t } = useTranslation();

  const millisecondsDuration =
    DateTime.local()
      .diff(DateTime.fromISO(date))
      .shiftTo("minutes", "seconds")
      .get("minutes") *
    60 *
    1000;

  return (
    <tr>
      <td>{name}</td>
      <td className={styles.shaded}>{upvoteTotal}</td>
      <td className={styles.shaded}>{voteTypeCaption(decision)}</td>
      <td className={styles.shaded}>{hash}</td>
      <td className={styles.shaded}>
        {t("proposals.bakersTable.timeAgo", {
          value: {
            date,
            milliseconds: millisecondsDuration,
            format: "dd MMM",
            withYearFormat: "dd MMM yyyy",
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
          <th className={styles.upvoteTotal}>
            {t("proposals.bakersTable.header.votesAmount")}
          </th>
          <th className={styles.upvoteType}>
            {t("proposals.bakersTable.header.votesType")}
          </th>
          <th className={styles.hash}>
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
            <BakersTableItem
              key={index}
              name={item.author.name}
              upvoteTotal={item.author.rolls}
              decision={item.decision}
              hash={item.operation}
              date={item.timestamp}
            />
          )
        )}
      </tbody>
    </table>
  );
};

export default BakersTable;
