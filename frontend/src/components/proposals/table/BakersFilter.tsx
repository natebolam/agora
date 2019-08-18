import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/table/BakersFilter.scss";
import { useTranslation } from "react-i18next";
import { BallotsStats } from "~/models/Period";
import { Decision } from "~/models/Decision";
import SvgDownIcon from "~/assets/svg/DownIcon";
import SvgUpIcon from "~/assets/svg/UpIcon";
import SvgPassIcon from "~/assets/svg/PassIcon";

interface BakersFilterButtonTypes {
  className?: string;
  icon: ReactElement;
  percent?: string;
  total?: number;
  caption: string;
  selected?: boolean;
  onClick?: () => void;
}

const BakersFilterButton: FunctionComponent<BakersFilterButtonTypes> = ({
  icon,
  percent = "0.00",
  total = 0,
  caption = false,
  selected,
  onClick = (): void => {},
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <button
      className={cx(styles.filter__button, { [styles.selected]: selected })}
      onClick={onClick}
    >
      {icon}
      <span>
        <b>
          {t("proposals.bakersTable.filter.buttonVotes", { percent, total })}
        </b>
        {caption}
      </span>
    </button>
  );
};

type FilterChangeHandler = (value: Decision[]) => void;

interface BakersFilterTypes {
  className?: string;
  ballots: BallotsStats;
  filter: Decision[];
  onFilterChange?: FilterChangeHandler;
}

const BakersFilter: FunctionComponent<BakersFilterTypes> = ({
  className,
  ballots,
  filter,
  onFilterChange = (): void => {},
}): ReactElement => {
  const { t } = useTranslation();
  const [decisions, setDecisions] = useState(filter);
  const onePercent = (ballots.yay + ballots.nay + ballots.pass) / 100;

  const handleFilterChange = (filterType: Decision): (() => void) => {
    return (): void => {
      const copy = [...decisions];
      const idx = copy.indexOf(filterType);
      if (idx > -1) copy.splice(idx, 1);
      else copy.push(filterType);

      setDecisions(copy);
      onFilterChange(copy);
    };
  };

  return (
    <div className={cx(className, styles.filter)}>
      <BakersFilterButton
        total={ballots.yay}
        percent={(ballots.yay / onePercent).toFixed(2)}
        icon={<SvgUpIcon />}
        caption={t("proposals.bakersTable.filter.inFavorCaption")}
        selected={decisions.includes("yay")}
        onClick={handleFilterChange("yay")}
      />
      <BakersFilterButton
        total={ballots.nay}
        percent={(ballots.nay / onePercent).toFixed(2)}
        icon={<SvgDownIcon />}
        caption={t("proposals.bakersTable.filter.againstCaption")}
        selected={decisions.includes("nay")}
        onClick={handleFilterChange("nay")}
      />
      <BakersFilterButton
        total={ballots.pass}
        percent={(ballots.pass / onePercent).toFixed(2)}
        icon={<SvgPassIcon />}
        caption={t("proposals.bakersTable.filter.passCaption")}
        selected={decisions.includes("pass")}
        onClick={handleFilterChange("pass")}
      />
    </div>
  );
};

export default BakersFilter;
