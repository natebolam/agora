import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/common/Button.scss";
import { Link } from "react-navi";
import { LinkProps } from "react-navi/dist/types/Link";

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
