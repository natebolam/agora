import React, { FunctionComponent } from "react";
import cx from "classnames";
import styles from "~/styles/components/controls/AgoraSelect.scss";

interface AgoraSelectOptionTypes {
  option: any;
  captionKey: string;
  valueKey: string;
}

const AgoraSelectOption: FunctionComponent<AgoraSelectOptionTypes>
  = (props) => {
  return (
    <option>
      {props.option[props.captionKey]}
    </option>
  );
};

interface TezosSelectTypes {
  className?: string;
  options: any[];
  captionKey?: string;
  valueKey?: string;
}

const AgoraSelect: FunctionComponent<TezosSelectTypes> = (props) => {
  const { options, captionKey, valueKey } = props;

  return (
    <select className={cx(props.className, styles.agoraSelect)}>
      {
        options.map((value: any, index: number) => (
          <AgoraSelectOption
            option={value}
            captionKey={captionKey!}
            valueKey={valueKey!}
            key={index}
          />
        ))
      }
    </select>
  )
};

AgoraSelect.defaultProps = {
  options: [],
  captionKey: "caption",
  valueKey: "value",
};

export default AgoraSelect;