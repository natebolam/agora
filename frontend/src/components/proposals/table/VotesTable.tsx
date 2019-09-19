import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import PointerIconSvg from "~/assets/svg/PointerIcon";
import SvgUpIcon from "~/assets/svg/UpIcon";
import NoUserIcon from "./NoUserIcon";

interface VotesTableItemTypes {
  item: ProposalVotesListItem;
}

const VotesTableItem: FunctionComponent<VotesTableItemTypes> = ({
  item,
}): ReactElement => {
  const { t } = useTranslation();

  const name = (): JSX.Element | string => {
    const text = item.author.name ? item.author.name : item.author.pkh;
    const image = item.author.logoUrl ? (
      <img src={item.author.logoUrl} />
    ) : (
      <NoUserIcon className={styles.no_user} value={item.author.pkh} />
    );
    if (item.author.profileUrl)
      return (
        <a href={item.author.profileUrl}>
          {text}
          {image}
        </a>
      );
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
      <td className={styles.rolls}>{item.author.rolls}</td>
      <td className={styles.decision}>
        <a href={`https://tzstats.com/operation/${item.operation}`}>
          <SvgUpIcon />
        </a>
      </td>
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
        case "timestamp":
          decision =
            new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime();
          break;
      }
      return (decision || a.id - b.id) * sort.order;
    });
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
          <th className={styles.rolls} onClick={orderBy("rolls")}>
            {t("proposals.bakersTable.header.votesAmount")}
            {sort.field == "rolls" && (
              <PointerIconSvg
                className={sort.order == -1 ? styles.up : void 0}
              />
            )}
          </th>
          <th className={styles.decision}>
            {t("proposals.bakersTable.header.votesType")}
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
