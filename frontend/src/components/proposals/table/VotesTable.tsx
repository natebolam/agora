import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import PointerIconSvg from "~/assets/svg/PointerIcon";

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

type SortChangeHandler = (sort: { field: string; order: number }) => void;

interface VotesTableTypes {
  className?: string;
  data: ProposalVotesListItem[];
  onSortChange?: SortChangeHandler;
}

const VotesTable: FunctionComponent<VotesTableTypes> = ({
  className,
  data: initialData,
  onSortChange = (): void => {},
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
    onSortChange(newSort);
  };

  const sortedData = (): ProposalVotesListItem[] => {
    if (!sort.order) return initialData;

    return data.sort((a, b): number => {
      let decision = 0;
      switch (sort.field) {
        case "name":
          if (a.author.name && !b.author.name) decision = -1;
          if (!a.author.name && b.author.name) decision = 1;
          if (a.author.name && b.author.name)
            decision = a.author.name.localeCompare(b.author.name);
          if (!a.author.name && !b.author.name)
            decision = a.author.pkh.localeCompare(b.author.pkh);
          break;
        case "rolls":
          decision = a.author.rolls - b.author.rolls;
          break;
        case "operation":
          decision = a.operation.localeCompare(b.operation);
          break;
        case "timestamp":
          decision =
            new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime();
          break;
      }
      return (decision || a.id - b.id) * sort.order;
    });
  };

  return (
    <table className={cx(className, styles.bakers)}>
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
          <th className={styles.rolls} onClick={orderBy("rolls")}>
            {t("proposals.bakersTable.header.votesAmount")}
            {sort.field == "rolls" && (
              <PointerIconSvg
                className={sort.order == -1 ? styles.up : void 0}
              />
            )}
          </th>
          <th className={styles.operation} onClick={orderBy("operation")}>
            {t("proposals.bakersTable.header.hash")}
            {sort.field == "operation" && (
              <PointerIconSvg
                className={sort.order == -1 ? styles.up : void 0}
              />
            )}
          </th>
          <th className={styles.date} onClick={orderBy("timestamp")}>
            {t("proposals.bakersTable.header.date")}
            {sort.field == "timestamp" && (
              <PointerIconSvg
                className={sort.order == -1 ? styles.up : void 0}
              />
            )}
          </th>
        </tr>
      </thead>
      <tbody>
        {sortedData().map(
          (item: ProposalVotesListItem, index: number): ReactElement => (
            <VotesTableItem key={index} item={item} />
          )
        )}
      </tbody>
    </table>
  );
};

export default VotesTable;
