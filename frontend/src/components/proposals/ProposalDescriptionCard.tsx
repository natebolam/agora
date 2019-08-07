import React, { FunctionComponent, ReactElement } from "react";
import Card from "~/components/common/Card";
import styles from "~/styles/components/proposals/ProposalDescriptionCard.scss";
import { useTranslation } from "react-i18next";

interface ProposalDescriptionCardTypes {
  className?: string;
  content: string;
}

const ProposalDescriptionCard: FunctionComponent<
  ProposalDescriptionCardTypes
> = ({ className, content }): ReactElement => {
  const { t } = useTranslation();
  return (
    <Card className={className}>
      <div className={styles.proposalDescription__title}>
        {t("proposals.proposalDescription")}
      </div>
      <div
        className={styles.proposalDescription__body}
        dangerouslySetInnerHTML={{ __html: content }}
      />
    </Card>
  );
};

export default ProposalDescriptionCard;
