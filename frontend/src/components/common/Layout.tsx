import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/common/Layout.scss";

interface LayoutProps {
  children: React.ReactNode;
  className?: string;
}

export const Layout: FunctionComponent<LayoutProps> = ({
  children,
  className,
}): ReactElement => {
  return <div className={cx(className, styles.layout)}>{children}</div>;
};

export const LayoutContent: FunctionComponent<LayoutProps> = ({
  children,
  className,
}): ReactElement => {
  return (
    <div className={cx(className, styles.layout__content)}>{children}</div>
  );
};
