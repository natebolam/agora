import React from "react";
import styles from "~/styles/components/common/Layout.scss";

interface LayoutProps {
  children: React.ReactNode
}

export default function Layout({ children }: LayoutProps): React.FunctionComponentElement<LayoutProps> {
  return (
    <div className={styles.layout}>
      <div className={styles.layout__content}>
        {children}
      </div>
    </div>
  );
}