import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import PointerIconSvg from "~/assets/svg/PointerIcon";
import NoUserIcon from "./NoUserIcon";

interface VotesTableItemTypes {
  item: ProposalVotesListItem;
}

const VotesTableItem: FunctionComponent<VotesTableItemTypes> = ({
  item,
}): ReactElement => {
  const { t } = useTranslation();

  const name = (): JSX.Element | string => {
    const text = item.author ? item.author : item.author;
    const image = <NoUserIcon className={styles.no_user} value={item.author} />;
    return (
      <span>
        {text}
        {image}
      </span>
    );
  };

  return (
    <tr>
      <td className={styles.name}>{name()}</td>
      <td
        className={styles.date}
        title={t("proposals.bakersTable.time", {
          value: {
            date: item.timestamp,
            format: "hh:mm:ss dd MMM yyyy",
          },
        })}
      >
        {t("proposals.bakersTable.timeAgo", {
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
  const [sort, setSort] = useState({ field: "", order: 0 });
  const initialSort = { field: "", order: 0 };

  const orderBy = (field: string): (() => void) => (): void => {
    const newSort =
      sort.order == -1
        ? initialSort
        : { order: sort.field != field ? 1 : -1, field };
    setSort(newSort);
  };

  return (
    <table className={cx(className, styles.votes)}>
      <thead>
        <tr>
          <th className={styles.name} onClick={orderBy("name")}>
            {t("proposals.bakersTable.header.baker")}
            {sort.field == "name" && (
              <PointerIconSvg
                className={sort.order == -1 ? styles.up : void 0}
              />
            )}
          </th>
          <th className={styles.date} onClick={orderBy("timestamp")}>
            {t("proposals.bakersTable.header.time")}
            {sort.field == "timestamp" && (
              <PointerIconSvg
                className={sort.order == -1 ? styles.up : void 0}
              />
            )}
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
