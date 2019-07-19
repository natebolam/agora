import React, { FunctionComponent, ReactElement } from "react";
import { TestingPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import PeriodHeader from "~/components/proposals/PeriodHeader";
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
      <LayoutContent>
        <PeriodHeader
          currentStage="testing"
          period={period.period}
          totalPeriods={period.totalPeriods}
        />
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
      <div className={styles.bakers__background}>
        <LayoutContent>
          <ProposalDescriptionCard
            className={styles.testing__proposalCard}
            content={period.proposal.longDescription}
          />
        </LayoutContent>
      </div>
    </>
  );
};

export default TestingView;
