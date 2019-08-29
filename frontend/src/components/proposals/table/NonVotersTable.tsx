import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersTable.scss";
import { useTranslation } from "react-i18next";
import { Proposer } from "~/models/ProposalInfo";
import { images } from "~/assets/mtb_logos/images";
import NoUserIcon from "./NoUserIcon";

interface NonVotersTableItemTypes {
  item: Proposer;
}

const NonVotersTableItem: FunctionComponent<NonVotersTableItemTypes> = ({
  item,
}): ReactElement => {
  const name = (): JSX.Element | string => {
    const text = item.name ? item.name : item.pkh;
    const image = item.logoUrl ? (
      <img src={images[item.logoUrl]} />
    ) : (
      <NoUserIcon className={styles.no_user} value={item.pkh} />
    );
    if (item.profileUrl)
      return (
        <a href={item.profileUrl}>
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
      <td className={styles.rolls}>{item.rolls}</td>
    </tr>
  );
};

interface NonVotersTableTypes {
  className?: string;
  data: Proposer[];
}

const NonVotersTable: FunctionComponent<NonVotersTableTypes> = ({
  className,
  data,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <table className={cx(className, styles.nonvoters)}>
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
          (item: Proposer, index: number): ReactElement => (
            <NonVotersTableItem key={index} item={item} />
          )
        )}
      </tbody>
    </table>
  );
};

export default NonVotersTable;
