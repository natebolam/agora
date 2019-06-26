import React, { FunctionComponent } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/BakersTable.scss";

interface BakersTableItemTypes {
  name: string;
  upvoteTotal: number;
  upvotePercent: number;
  upvoteTitles: string;
  hash: string;
  date: Date;
}

const BakersTableItem: FunctionComponent<BakersTableItemTypes>
  = ({name, upvoteTotal, upvotePercent, upvoteTitles, hash, date}) => {
  return (
    <tr>
      <td>
        {name}
      </td>
      <td className={styles.shaded}>
        {upvoteTotal} ({upvotePercent}%)
      </td>
      <td className={styles.shaded}>
        {upvoteTitles}
      </td>
      <td className={styles.shaded}>
        {hash}
      </td>
      <td className={styles.shaded}>
        5 Minutes ago
      </td>
    </tr>
  )
};

interface BakersTableTypes {
  className?: string;
}

const BakersTable: FunctionComponent<BakersTableTypes>
  = ({ className }) => {
  const item: BakersTableItemTypes = {
    name: "Tezos Capital Legacy",
    upvoteTotal: 1200,
    upvotePercent: 12,
    upvoteTitles: "Brasilia A",
    hash: "ooAPKpRGXdMqR7ir76dhPS4qQ2cdwg9L",
    date: new Date(),
  };
  return (
    <table className={cx(className, styles.bakers)}>
      <thead>
      <tr>
        <th>Bakers</th>
        <th>Upvotes</th>
        <th>Upvote(s)</th>
        <th>Operation(s)</th>
        <th>Date</th>
      </tr>
      </thead>
      <tbody>
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
        <BakersTableItem
          name={item.name}
          upvoteTotal={item.upvoteTotal}
          upvotePercent={item.upvotePercent}
          upvoteTitles={item.upvoteTitles}
          hash={item.hash}
          date={item.date}
        />
      </tbody>
    </table>
  );
};

export default BakersTable;