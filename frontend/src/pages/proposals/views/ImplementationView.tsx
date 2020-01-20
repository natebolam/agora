import React, { FunctionComponent, ReactElement } from "react";
import { ImplementationStageInfo } from "~/models/Stage";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/TestingStagePage.scss";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import TestingCountdown from "~/components/proposals/TestingCountdown";
import ProposalDescriptionCard from "~/components/proposals/ProposalDescriptionCard";
import { useTranslation } from "react-i18next";
import { DateTime } from "luxon";

interface ImplementationViewProps {
  stage: ImplementationStageInfo;
}

const ImplementationView: FunctionComponent<ImplementationViewProps> = ({
  stage,
}): ReactElement => {
  const { t } = useTranslation();
  const dateFrom = DateTime.local(2020, Math.floor(stage.stage / 4) + 1, 22);
  const dateTo = dateFrom.endOf("month");

  return (
    <>
      <LayoutContent className={styles.stage__primaryInfo}>
        <div>
          <ProposalDescription
            className={styles.testing__description}
            title={
              stage.proposal.title ? stage.proposal.title : stage.proposal.hash
            }
            description={
              stage.proposal.shortDescription
                ? stage.proposal.shortDescription
                : t("proposals.common.noShortDescriptionCaption")
            }
            discourseLink={stage.proposal.discourseLink}
            learnMoreLink={`/proposal/${stage.proposal.stage}/${stage.proposal.id}`}
          />
          <TestingCountdown
            className={styles.testing__countdown}
            dateFrom={dateFrom.toISO()}
            dateTo={dateTo.toISO()}
          />
        </div>
      </LayoutContent>
      <LayoutContent className={styles.stage__secondaryInfo}>
        <ProposalDescriptionCard
          className={styles.testing__proposalCard}
          content={
            stage.proposal.longDescription
              ? stage.proposal.longDescription
              : t("proposals.common.noLongDescriptionCaption")
          }
        />
      </LayoutContent>
    </>
  );
};

export default ImplementationView;
