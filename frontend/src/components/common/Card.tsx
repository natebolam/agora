import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/common/Card.scss";

interface Props {
  header?: React.ReactNode;
  body?: React.ReactNode;
  children?: React.ReactNode;
  className?: string;
  bodyClassName?: string;
}

const Card: FunctionComponent<Props> = ({
  header,
  body,
  children,
  className,
  bodyClassName,
}): ReactElement => {
  const HeaderElement = header ? (
    <div className={styles.card__header}>{header}</div>
  ) : null;
  return (
    <div className={cx(className, styles.card)}>
      {HeaderElement}
      <div className={cx(bodyClassName, styles.card__body)}>
        {body ? body : children}
      </div>
    </div>
  );
};

export default Card;
