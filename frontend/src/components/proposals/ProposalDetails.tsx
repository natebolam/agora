import React, { FunctionComponent, ReactElement } from "react";
import Card from "~/components/common/Card";
import styles from "~/styles/components/proposals/ProposalDetails.scss";
import { Proposal } from "~/models/Period";
import { useTranslation } from "react-i18next";

interface ProposalDetailsTypes {
  className?: string;
  proposal: Proposal;
}

const ProposalDetails: FunctionComponent<ProposalDetailsTypes> = ({
  className,
  proposal,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <Card
      className={className}
      bodyClassName={styles.proposalDetails}
      header={"Details"}
    >
      <div className={styles.proposalDetails__titles}>
        <div>{t("proposals.details.timeTitle")}</div>
        <div>{t("proposals.details.hashTitle")}</div>
        <div>{t("proposals.details.proposerTitle")}</div>
        {proposal.proposalFile && (
          <div>{t("proposals.details.proposalFileTitle")}</div>
        )}
      </div>
      <div className={styles.proposalDetails__values}>
        <div>
          {t("proposals.details.timeCaption", {
            value: {
              date: proposal.timeCreated,
              format: "MMMM dd, yyyy - hh:mm ZZZZ",
            },
          })}
        </div>
        <div>{proposal.hash}</div>
        <div>{proposal.proposer.pkh}</div>
        {proposal.proposalFile && <div>{proposal.proposalFile}</div>}
      </div>
    </Card>
  );
};

export default ProposalDetails;
