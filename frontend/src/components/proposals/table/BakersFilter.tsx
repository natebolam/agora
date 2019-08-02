import React, { FunctionComponent, ReactElement } from "react";
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
  percent?: number;
  total?: number;
  caption: string;
  selected?: boolean;
  onClick?: () => void;
}

const BakersFilterButton: FunctionComponent<BakersFilterButtonTypes> = ({
  icon,
  percent = 0,
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

type FilterChangeHandler = (value?: Decision) => void;

interface BakersFilterTypes {
  className?: string;
  ballots: BallotsStats;
  filter?: Decision;
  onFilterChange?: FilterChangeHandler;
}

const BakersFilter: FunctionComponent<BakersFilterTypes> = ({
  className,
  ballots,
  filter,
  onFilterChange = (): void => {},
}): ReactElement => {
  const { t } = useTranslation();
  const onePercent = (ballots.yay + ballots.nay + ballots.pass) / 100;

  const handleFilterChange = (filterType: Decision): (() => void) => {
    return (): void =>
      onFilterChange(filterType === filter ? undefined : filterType);
  };

  return (
    <div className={cx(className, styles.filter)}>
      <BakersFilterButton
        total={ballots.yay}
        percent={parseInt((ballots.yay / onePercent).toFixed(0))}
        icon={<SvgUpIcon />}
        caption={t("proposals.bakersTable.filter.inFavorCaption")}
        selected={filter === "yay"}
        onClick={handleFilterChange("yay")}
      />
      <BakersFilterButton
        total={ballots.nay}
        percent={parseInt((ballots.nay / onePercent).toFixed(0))}
        icon={<SvgDownIcon />}
        caption={t("proposals.bakersTable.filter.againstCaption")}
        selected={filter === "nay"}
        onClick={handleFilterChange("nay")}
      />
      <BakersFilterButton
        total={ballots.pass}
        percent={parseInt((ballots.pass / onePercent).toFixed(0))}
        icon={<SvgPassIcon />}
        caption={t("proposals.bakersTable.filter.passCaption")}
        selected={filter === "pass"}
        onClick={handleFilterChange("pass")}
      />
    </div>
  );
};

export default BakersFilter;
