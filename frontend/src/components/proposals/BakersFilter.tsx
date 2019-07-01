import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import ThumbsUpIcon from "~/assets/png/thumbs_up_icon.png";
import ThumbsDownIcon from "~/assets/png/thumbs_down_icon.png";
import PassIcon from "~/assets/png/pass_icon.png";
import styles from "~/styles/components/proposals/BakersFilter.scss";

interface BakersFilterButtonTypes {
  className?: string;
  iconSrc: string;
  percent?: number;
  total?: number;
  caption: string;
  selected?: boolean;
  onClick?: () => void;
}

const BakersFilterButton: FunctionComponent<BakersFilterButtonTypes>
  = ({ iconSrc, percent, total, caption, selected, onClick }) => {
  return (
    <button
      className={cx(styles.filter__button, { [styles.selected]: selected })}
      onClick={onClick}>
      <img src={iconSrc}/>
      <b>
        {percent}% ({total})
      </b>
      {caption}
    </button>
  );
};

BakersFilterButton.defaultProps = {
  percent: 0,
  total: 0,
  selected: false,
  onClick: () => {
  }
};

interface BakersFilterTypes {
  className?: string;
}

const BakersFilter: FunctionComponent<BakersFilterTypes>
  = ({ className }): ReactElement => {

  const [selected, setSelected] = useState(0);

  return (
    <div className={cx(className, styles.filter)}>
      <BakersFilterButton
        iconSrc={ThumbsUpIcon}
        caption="In Favor"
        selected={selected === 0}
        onClick={() => setSelected(0)}
      />
      <BakersFilterButton
        iconSrc={ThumbsDownIcon}
        caption="Against"
        selected={selected === 1}
        onClick={() => setSelected(1)}
      />
      <BakersFilterButton
        iconSrc={PassIcon}
        caption="Pass"
        selected={selected === 2}
        onClick={() => setSelected(2)}
      />
    </div>
  );
};

export default BakersFilter;
