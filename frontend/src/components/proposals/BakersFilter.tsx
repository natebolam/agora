import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import ThumbsUpIcon from "~/assets/png/thumbs_up_icon.png";
import ThumbsDownIcon from "~/assets/png/thumbs_down_icon.png";
import PassIcon from "~/assets/png/pass_icon.png";
import styles from "~/styles/components/proposals/BakersFilter.scss";
import { useTranslation } from "react-i18next";

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

interface BakersFilterTypes {
  className?: string;
}

const BakersFilter: FunctionComponent<BakersFilterTypes> = ({
  className,
}): ReactElement => {
  const { t } = useTranslation();
  const [selected, setSelected] = useState(0);

  return (
    <div className={cx(className, styles.filter)}>
      <BakersFilterButton
        iconSrc={ThumbsUpIcon}
        caption={t("proposals.bakersTable.filter.inFavorCaption")}
        selected={selected === 0}
        onClick={(): void => setSelected(0)}
      />
      <BakersFilterButton
        iconSrc={ThumbsDownIcon}
        caption={t("proposals.bakersTable.filter.againstCaption")}
        selected={selected === 1}
        onClick={(): void => setSelected(1)}
      />
      <BakersFilterButton
        iconSrc={PassIcon}
        caption={t("proposals.bakersTable.filter.passCaption")}
        selected={selected === 2}
        onClick={(): void => setSelected(2)}
      />
    </div>
  );
};

export default BakersFilter;
