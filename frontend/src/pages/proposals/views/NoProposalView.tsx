import React, { FunctionComponent, ReactElement } from "react";
import styles from "~/styles/pages/proposals/NoProposalView.scss";
import { useTranslation } from "react-i18next";

interface NoProposalViewProps {
  isProposalStage: boolean;
}

const NoProposalView: FunctionComponent<NoProposalViewProps> = ({
  isProposalStage,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <div className={styles.noProposals}>
      {isProposalStage
        ? t("proposals.common.noProposalsCaption")
        : t("proposals.common.noCycleProposalsCaption")}
    </div>
  );
};

export default NoProposalView;
