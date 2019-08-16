import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";
import { ProposalBallotsListItem } from "~/models/ProposalBallotsList";
import PointerIconSvg from "~/assets/svg/PointerIcon";

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
  data: initialData,
}): ReactElement => {
  const { t } = useTranslation();
  const data = [...initialData];
  const [sort, setSort] = useState({ field: "", order: 0 });
  const initialSort = { field: "", order: 0 };

  const orderBy = (field: string): (() => void) => (): void => {
    if (sort.order == -1) return setSort(initialSort);
    setSort({ order: sort.field != field ? 1 : -1, field });
  };

  const sortedData = (): ProposalBallotsListItem[] => {
    if (!sort.order) return initialData;

    return data.sort((a, b): number => {
      switch (sort.field) {
        case "name":
          const nameA = a.author.name ? a.author.name : a.author.pkh;
          const nameB = b.author.name ? b.author.name : b.author.pkh;
          return nameA.localeCompare(nameB) * sort.order;
        case "rolls":
          return (a.author.rolls - b.author.rolls) * sort.order;
        case "decision":
          const decisionOrder = ["yay", "nay", "pass"];
          return (
            (decisionOrder.indexOf(a.decision) -
              decisionOrder.indexOf(b.decision)) *
            sort.order
          );
        case "operation":
          return a.operation.localeCompare(b.operation) * sort.order;
        case "timestamp":
          return (
            (new Date(a.timestamp).getTime() -
              new Date(b.timestamp).getTime()) *
            sort.order
          );
      }
      return 0;
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
          <th className={styles.decision} onClick={orderBy("decision")}>
            {t("proposals.bakersTable.header.votesType")}
            {sort.field == "decision" && (
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
          (item: ProposalBallotsListItem, index: number): ReactElement => (
            <BakersTableItem key={index} item={item} />
          )
        )}
      </tbody>
    </table>
  );
};

export default BakersTable;
