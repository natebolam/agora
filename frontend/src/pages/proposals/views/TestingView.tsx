import React, { FunctionComponent, ReactElement } from "react";
import { TestingPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/TestingStagePage.scss";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import TestingCountdown from "~/components/proposals/TestingCountdown";
import ProposalDescriptionCard from "~/components/proposals/ProposalDescriptionCard";
import { useTranslation } from "react-i18next";

interface TestingViewProps {
  period: TestingPeriodInfo;
}

const TestingView: FunctionComponent<TestingViewProps> = ({
  period,
}): ReactElement => {
  const { t } = useTranslation();
  return (
    <>
      <LayoutContent className={styles.period__primaryInfo}>
        <div>
          <ProposalDescription
            className={styles.testing__description}
            title={
              period.proposal.title
                ? period.proposal.title
                : period.proposal.hash
            }
            description={
              period.proposal.shortDescription
                ? period.proposal.shortDescription
                : t("proposals.common.noDescriptionCaption")
            }
          />
          <TestingCountdown
            className={styles.testing__countdown}
            dateFrom={period.period.startTime}
            dateTo={period.period.endTime}
          />
        </div>
      </LayoutContent>
      <LayoutContent className={styles.period__secondaryInfo}>
        <ProposalDescriptionCard
          className={styles.testing__proposalCard}
          content={
            period.proposal.longDescription
              ? period.proposal.longDescription
              : t("proposals.common.noDescriptionCaption")
          }
        />
      </LayoutContent>
    </>
  );
};

export default TestingView;
