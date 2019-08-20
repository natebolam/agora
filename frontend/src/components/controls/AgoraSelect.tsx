import React, {
  FunctionComponent,
  ReactElement,
  useEffect,
  useRef,
  useState,
  createRef,
  RefObject,
} from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/AgoraSelect.scss";
import ArrowBottomSvg from "~/assets/svg/ArrowBottom";
import Scrollbars from "react-custom-scrollbars";
import SelectedItem from "~/assets/svg/SelectedItem";
import { PeriodTime } from "~/models/Period";
import { useTranslation } from "react-i18next";

export interface AgoraSelectDataItem {
  value: number;
  periodTime: PeriodTime;
}

interface AgoraSelectOptionTypes {
  option: AgoraSelectDataItem;
  selected: boolean;
  onSelect: (value: AgoraSelectDataItem) => void;
}

const AgoraSelectOption: FunctionComponent<AgoraSelectOptionTypes> = ({
  option,
  selected,
  onSelect,
}): ReactElement => {
  const { t } = useTranslation();
  const handleSelect = (): void => onSelect(option);
  const ref: RefObject<HTMLSpanElement> = createRef();

  const getType = (periodTimes: PeriodTime): string => {
    switch (periodTimes.periodType) {
      case "promotion_vote":
        return "Promotion";
      case "proposal":
        return "Proposal";
      case "testing":
        return "Testing";
      case "testing_vote":
        return "Exploration";
    }
  };

  const data = {
    value: option.value,
    periodType: getType(option.periodTime),
    startTime: {
      date: option.periodTime.startTime,
      format: "MM:dd:yy",
    },
    endTime: {
      date: option.periodTime.endTime,
      format: "MM:dd:yy",
    },
  };

  const dCaption = t("proposals.periodSelect.captionDate", data).replace(
    /:/g,
    "/"
  );
  const mCaption = t("proposals.periodSelect.captionDateMobile", data).replace(
    /:/g,
    "/"
  );

  const getCaption = (): string =>
    window.innerWidth > 850 ? dCaption : mCaption;

  useEffect((): (() => void) => {
    const handleResize = (): void => {
      const span = ref.current as HTMLElement;
      span.textContent = getCaption();
    };

    window.addEventListener("resize", handleResize);
    return (): void => {
      window.removeEventListener("resize", handleResize);
    };
  }, []);
  return (
    <div className={styles.agoraSelect__menu__item} onClick={handleSelect}>
      <span ref={ref}>{getCaption()}</span>
      {selected && <SelectedItem />}
    </div>
  );
};

const AgoraThumb: FunctionComponent = (): ReactElement => {
  return <div className={styles.agoraSelect__thumb} />;
};

interface AgoraSelectListTypes {
  className?: string;
  options: AgoraSelectDataItem[];
  value: AgoraSelectDataItem;
  onSelect: (value: AgoraSelectDataItem) => void;
}

const AgoraSelectList: FunctionComponent<AgoraSelectListTypes> = ({
  className,
  options,
  value,
  onSelect,
}): ReactElement => {
  return (
    <div className={cx(className, styles.agoraSelect__menu)}>
      <Scrollbars
        autoHeight
        renderThumbVertical={AgoraThumb}
        autoHeightMin={320}
      >
        {options.map(
          (item: AgoraSelectDataItem, index: number): ReactElement => (
            <AgoraSelectOption
              key={index}
              selected={value.value === item.value}
              option={item}
              onSelect={onSelect}
            />
          )
        )}
      </Scrollbars>
    </div>
  );
};

interface AgoraSelectTypes {
  options: AgoraSelectDataItem[];
  className?: string;
  value: AgoraSelectDataItem;
  onSelect?: (value: AgoraSelectDataItem) => void;
}

const AgoraSelect: FunctionComponent<AgoraSelectTypes> = ({
  className,
  options,
  value,
  onSelect = (): void => {},
}): ReactElement => {
  const [isOpen, setOpen] = useState(false);
  const ref = useRef<HTMLDivElement>(null);

  const handleItemSelect = (data: AgoraSelectDataItem): void => {
    setOpen(false);
    onSelect(data);
  };

  const handleSelectClick = (): void => setOpen(!isOpen);

  useEffect((): (() => void) => {
    const listener = (event: MouseEvent): void => {
      if (
        ref.current &&
        event.target instanceof Node &&
        !ref.current.contains(event.target)
      ) {
        setOpen(false);
      }
    };
    document.addEventListener("mouseup", listener);
    return (): void => {
      document.removeEventListener("mouseup", listener);
    };
  }, []);

  return (
    <div className={cx(className, styles.agoraSelect__wrapper)} ref={ref}>
      <div
        className={cx(styles.agoraSelect, { [styles.open]: isOpen })}
        onClick={handleSelectClick}
      >
        <div>{value.value}</div>
        <ArrowBottomSvg />
      </div>
      <AgoraSelectList
        className={isOpen ? null : styles.hidden}
        options={options}
        value={value}
        onSelect={handleItemSelect}
      />
    </div>
  );
};

export default AgoraSelect;
