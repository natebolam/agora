import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/common/Button.scss";
import { Link, LinkProps } from "react-router-dom";

interface ButtonProps extends LinkProps {
  className?: string;
  children?: React.ReactNode;
}

export const ButtonLink: FunctionComponent<ButtonProps> = ({
  className,
  children,
  ...linkProps
}): ReactElement => {
  return (
    <Link className={cx(className, styles.button)} {...linkProps}>
      {children}
    </Link>
  );
};
