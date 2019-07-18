import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";

interface BakersTableItemTypes {
  name: string;
  upvoteTotal: number;
  upvoteType: boolean;
  hash: string;
  date: string;
}

const BakersTableItem: FunctionComponent<BakersTableItemTypes> = ({
  name,
  upvoteTotal,
  upvoteType,
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
      <td className={styles.shaded}>
        {upvoteType
          ? t("proposals.bakersTable.upvoteTrue")
          : t("proposals.bakersTable.upvoteFalse")}
      </td>
      <td className={styles.shaded}>{hash}</td>
      <td className={styles.shaded}>
        {t("proposals.bakersTable.timeAgo", {
          value: {
            milliseconds: millisecondsDuration,
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
}

const BakersTable: FunctionComponent<BakersTableTypes> = ({
  className,
}): ReactElement => {
  const { t } = useTranslation();

  const item: BakersTableItemTypes = {
    name: "Tezos Capital Legacy",
    upvoteTotal: 1200,
    upvoteType: true,
    hash: "ooAPKpRGXdMqR7ir76dhPS4qQ2cdwg9LooAPKpRGXdMqR7ir76dhPS4qQ2cdwg9L",
    date: DateTime.local()
      .minus({ minutes: 10, days: 2 })
      .toISO(),
  };
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
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvoteType={item.upvoteType}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvoteType={item.upvoteType}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvoteType={item.upvoteType}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvoteType={item.upvoteType}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvoteType={item.upvoteType}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvoteType={item.upvoteType}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvoteType={item.upvoteType}
          hash={item.hash}
          date={item.date}
        />
      </tbody>
    </table>
  );
};

export default BakersTable;
