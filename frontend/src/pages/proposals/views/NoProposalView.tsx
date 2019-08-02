import React, { FunctionComponent, ReactElement } from "react";
import styles from "~/styles/pages/proposals/NoProposalView.scss";
import { useTranslation } from "react-i18next";

const NoProposalView: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={styles.noProposals}>
      {t("proposals.common.noProposalsCaption")}
    </div>
  );
};

export default NoProposalView;
