import React, { FunctionComponent } from "react";
import cx from "classnames";
import styles from "~/styles/components/common/Card.scss";

interface AgoraCardTypes {
  header?: React.ReactNode,
  body?: React.ReactNode,
  className?: string;
}

const Card: FunctionComponent<AgoraCardTypes>
  = ({header, body, className}) => {
  return (
    <div className={cx(className, styles.card)}>
      <div className={styles.card__header}>
        {header}
      </div>
      <div className={styles.card__body}>
        {body}
      </div>
    </div>
  )
};

export default Card;