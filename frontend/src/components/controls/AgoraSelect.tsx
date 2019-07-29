import React, {
  FunctionComponent,
  ReactElement,
  ReactNode,
  useEffect,
  useRef,
  useState,
} from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/AgoraSelect.scss";
import ArrowBottomSvg from "~/assets/svg/ArrowBottom";
import Scrollbars from "react-custom-scrollbars";
import SelectedItem from "~/assets/svg/SelectedItem";

export interface AgoraSelectDataItem {
  value: number;
  caption: ReactNode;
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
  const handleSelect = (): void => {
    onSelect(option);
  };
  return (
    <div className={styles.agoraSelect__menu__item} onClick={handleSelect}>
      {option.caption}
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
        <div>{value.caption}</div>
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
