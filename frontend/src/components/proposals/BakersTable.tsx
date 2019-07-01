import React, { FunctionComponent } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/BakersTable.scss";
import { useTranslation } from "react-i18next";

interface BakersTableItemTypes {
  name: string;
  upvoteTotal: number;
  upvoteType: boolean;
  hash: string;
  date: Date;
}

const BakersTableItem: FunctionComponent<BakersTableItemTypes>
  = ({name, upvoteTotal, upvoteType, hash, date}) => {
  const { t } = useTranslation();

  return (
    <tr>
      <td>
        {name}
      </td>
      <td className={styles.shaded}>
        {upvoteTotal}
      </td>
      <td className={styles.shaded}>
        {upvoteType ?
          t('proposals.bakers_table.upvote_true')
          :
          t('proposals.bakers_table.upvote_false')
        }
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

  const { t } = useTranslation();

  const item: BakersTableItemTypes = {
    name: "Tezos Capital Legacy",
    upvoteTotal: 1200,
    upvoteType: true,
    hash: "ooAPKpRGXdMqR7ir76dhPS4qQ2cdwg9LooAPKpRGXdMqR7ir76dhPS4qQ2cdwg9L",
    date: new Date(),
  };
  return (
    <table className={cx(className, styles.bakers)}>
      <thead>
      <tr>
        <th className={styles.name}>{t('proposals.bakers_table.header.baker')}</th>
        <th className={styles.upvoteTotal}>{t('proposals.bakers_table.header.votes_amount')}</th>
        <th className={styles.upvoteType}>{t('proposals.bakers_table.header.votes_type')}</th>
        <th className={styles.hash}>{t('proposals.bakers_table.header.hash')}</th>
        <th className={styles.date}>{t('proposals.bakers_table.header.date')}</th>
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
