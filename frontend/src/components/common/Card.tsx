import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/common/Card.scss";

interface AgoraCardTypes {
  header?: React.ReactNode,
  body?: React.ReactNode,
  className?: string;
}

const Card: FunctionComponent<AgoraCardTypes>
  = ({header, body, children, className}) => {

  const HeaderElement = header ?
    <div className={styles.card__header}>
      {header}
    </div>
    :
    null;

  return (
    <div className={cx(className, styles.card)}>
      {HeaderElement}
      <div className={styles.card__body}>
        {body ? body : children}
      </div>
    </div>
  )
};

export default Card;
