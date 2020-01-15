import React, { FunctionComponent, ReactElement } from "react";
import Card from "~/components/common/Card";
import styles from "~/styles/components/proposals/ProposalDetails.scss";
import { useTranslation } from "react-i18next";
import { Proposal, Url } from "~/models/ProposalInfo";

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
        <div>{t("proposals.details.urlsTitle")}</div>
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
        <div>
          {proposal.urls.map(
            (url: Url): ReactElement => (
              <>
                <a
                  href={
                    url.url.startsWith("http://") ||
                    url.url.startsWith("https://")
                      ? url.url
                      : "//" + url.url
                  }
                >{`${url.description}: ${url.hash}`}</a>
                <br></br>
              </>
            )
          )}
        </div>
      </div>
    </Card>
  );
};

export default ProposalDetails;
