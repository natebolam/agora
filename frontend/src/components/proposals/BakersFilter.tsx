import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import ThumbsUpIcon from "~/assets/png/thumbs_up_icon.png";
import ThumbsDownIcon from "~/assets/png/thumbs_down_icon.png";
import PassIcon from "~/assets/png/pass_icon.png";
import styles from "~/styles/components/proposals/BakersFilter.scss";
import { useTranslation } from "react-i18next";
import { BallotsStats } from "~/models/Period";
import { Decision } from "~/models/Decision";

interface BakersFilterButtonTypes {
  className?: string;
  iconSrc: string;
  percent?: number;
  total?: number;
  caption: string;
  selected?: boolean;
  onClick?: () => void;
}

const BakersFilterButton: FunctionComponent<BakersFilterButtonTypes> = ({
  iconSrc,
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
      <img alt="" src={iconSrc} />
      <b>{t("proposals.bakersTable.filter.buttonVotes", { percent, total })}</b>
      {caption}
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
        iconSrc={ThumbsUpIcon}
        caption={t("proposals.bakersTable.filter.inFavorCaption")}
        selected={filter === "yay"}
        onClick={handleFilterChange("yay")}
      />
      <BakersFilterButton
        total={ballots.nay}
        percent={parseInt((ballots.nay / onePercent).toFixed(0))}
        iconSrc={ThumbsDownIcon}
        caption={t("proposals.bakersTable.filter.againstCaption")}
        selected={filter === "nay"}
        onClick={handleFilterChange("nay")}
      />
      <BakersFilterButton
        total={ballots.pass}
        percent={parseInt((ballots.pass / onePercent).toFixed(0))}
        iconSrc={PassIcon}
        caption={t("proposals.bakersTable.filter.passCaption")}
        selected={filter === "pass"}
        onClick={handleFilterChange("pass")}
      />
    </div>
  );
};

export default BakersFilter;
