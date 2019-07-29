import React, { FunctionComponent, ReactElement } from "react";
import { TestingPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/TestingStagePage.scss";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import TestingCountdown from "~/components/proposals/TestingCountdown";
import ProposalDescriptionCard from "~/components/proposals/ProposalDescriptionCard";

interface TestingViewProps {
  period: TestingPeriodInfo;
}

const TestingView: FunctionComponent<TestingViewProps> = ({
  period,
}): ReactElement => {
  return (
    <>
      <LayoutContent className={styles.period__primaryInfo}>
        <div className={styles.testing__info}>
          <ProposalDescription
            className={styles.exploration__description}
            title={period.proposal.title}
            description={period.proposal.shortDescription}
          />
          <TestingCountdown
            className={styles.testing__info__countdown}
            dateFrom={period.period.startTime}
            dateTo={period.period.endTime}
          />
        </div>
      </LayoutContent>
      <LayoutContent className={styles.period__secondaryInfo}>
        <ProposalDescriptionCard
          className={styles.testing__proposalCard}
          content={period.proposal.longDescription}
        />
      </LayoutContent>
    </>
  );
};

export default TestingView;
