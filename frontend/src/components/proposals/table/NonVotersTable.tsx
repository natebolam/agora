import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { ProposalBallotsListItem } from "~/models/ProposalBallotsList";

interface NonVotersTableItemTypes {
  item: ProposalBallotsListItem;
}

const NonVotersTableItem: FunctionComponent<NonVotersTableItemTypes> = ({
  item,
}): ReactElement => (
  <tr>
    <td className={styles.name}>
      {item.author.name ? item.author.name : item.author.pkh}
    </td>
    <td className={styles.rolls}>{item.author.rolls}</td>
  </tr>
);

interface NonVotersTableTypes {
  className?: string;
  data: ProposalBallotsListItem[];
}

const NonVotersTable: FunctionComponent<NonVotersTableTypes> = ({
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
        </tr>
      </thead>
      <tbody>
        {data.map(
          (item: ProposalBallotsListItem, index: number): ReactElement => (
            <NonVotersTableItem key={index} item={item} />
          )
        )}
      </tbody>
    </table>
  );
};

export default NonVotersTable;
