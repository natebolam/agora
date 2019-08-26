import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { ProposalBallotsListItem } from "~/models/ProposalBallotsList";
import PointerIconSvg from "~/assets/svg/PointerIcon";
import { images } from "~/assets/mtb_logos/images";
import SvgDownIcon from "~/assets/svg/DownIcon";
import SvgUpIcon from "~/assets/svg/UpIcon";
import SvgPassIcon from "~/assets/svg/PassIcon";
import { Decision } from "~/models/Decision";

interface BakersTableItemTypes {
  item: ProposalBallotsListItem;
}

const voteTypeCaption = (decision: Decision): JSX.Element => {
  switch (decision) {
    case "yay":
      return <SvgUpIcon />;
    case "nay":
      return <SvgDownIcon />;
    case "pass":
      return <SvgPassIcon />;
  }
};

const BakersTableItem: FunctionComponent<BakersTableItemTypes> = ({
  item,
}): ReactElement => {
  const { t } = useTranslation();

  const name = (): JSX.Element | string => {
    const text = item.author.name ? item.author.name : item.author.pkh;
    const image = item.author.logoUrl ? (
      <img src={images[item.author.logoUrl]} />
    ) : null;
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
          {voteTypeCaption(item.decision)}
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
        {t("proposals.bakersTable.timeAgoFull", {
          value: {
            date: item.timestamp,
            format: "DDDD 'at' t",
            timeFormat: "t",
          },
        })}
      </td>
    </tr>
  );
};

type SortChangeHandler = (sort: { field: string; order: number }) => void;

interface BakersTableTypes {
  className?: string;
  data: ProposalBallotsListItem[];
  onSortChange?: SortChangeHandler;
}

const BakersTable: FunctionComponent<BakersTableTypes> = ({
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

  const sortedData = (): ProposalBallotsListItem[] => {
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
        case "decision":
          const decisionOrder = ["yay", "nay", "pass"];
          decision =
            decisionOrder.indexOf(a.decision) -
            decisionOrder.indexOf(b.decision);
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
          <th className={styles.decision} onClick={orderBy("decision")}>
            {t("proposals.bakersTable.header.votesType")}
            {sort.field == "decision" && (
              <PointerIconSvg
                className={sort.order == -1 ? styles.up : void 0}
              />
            )}
          </th>
          <th className={styles.date} onClick={orderBy("timestamp")}>
            {t("proposals.bakersTable.header.bakersDate")}
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
