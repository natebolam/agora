import React, { FunctionComponent, ReactElement, ReactNode } from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/AgoraSelect.scss";

interface AgoraSelectOptionTypes {
  option: Record<string, ReactNode>;
  captionKey: string;
  valueKey: string;
}

const AgoraSelectOption: FunctionComponent<AgoraSelectOptionTypes> = (
  props
): ReactElement => {
  return <option>{props.option[props.captionKey]}</option>;
};

interface AgoraSelectTypes {
  options: Record<string, ReactNode>[];
  className?: string;
  captionKey?: string;
  valueKey?: string;
}

const AgoraSelect: FunctionComponent<AgoraSelectTypes> = (
  props
): ReactElement => {
  const { options, captionKey = "caption", valueKey = "value" } = props;

  return (
    <select className={cx(props.className, styles.agoraSelect)}>
      {options.map(
        (value: Record<string, ReactNode>, index: number): ReactElement => (
          <AgoraSelectOption
            option={value}
            captionKey={captionKey}
            valueKey={valueKey}
            key={index}
          />
        )
      )}
    </select>
  );
};

export default AgoraSelect;
