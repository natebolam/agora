import React, { FunctionComponent, ReactElement } from "react";
import styles from "~/styles/components/common/Layout.scss";

interface LayoutProps {
  children: React.ReactNode;
}

export const Layout: FunctionComponent<LayoutProps> = ({
  children,
}): ReactElement => {
  return <div className={styles.layout}>{children}</div>;
};

export const LayoutContent: FunctionComponent<LayoutProps> = ({
  children,
}): ReactElement => {
  return <div className={styles.layout__content}>{children}</div>;
};
