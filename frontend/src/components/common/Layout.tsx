import React, { FunctionComponent } from "react";
import styles from "~/styles/components/common/Layout.scss";

interface LayoutProps {
  children: React.ReactNode
}

export const Layout: FunctionComponent<LayoutProps> = ({children}) => {
  return (
    <div className={styles.layout}>
      {children}
    </div>
  )
};

export const LayoutContent: FunctionComponent<LayoutProps>
  = ({children}) => {
  return (
    <div className={styles.layout__content}>
      {children}
    </div>
  )
};