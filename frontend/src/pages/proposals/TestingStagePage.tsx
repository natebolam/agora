import React, { FunctionComponent, ReactElement } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import styles from "~/styles/pages/proposals/TestingStagePage.scss";
import TestingCountdown from "~/components/proposals/TestingCountdown";
import ProposalDescriptionCard from "~/components/proposals/ProposalDescriptionCard";
import { DateTime, Duration } from "luxon";

const TestingStagePage: FunctionComponent = (): ReactElement => {
  const proposalDescription = {
    title: "Brasilia",
    description:
      "New proposal reduces roll size and increases gas limit. " +
      "Proposer states in their announcement post that they " +
      "have performed extensive testing.",
  };

  const proposalDetailedDescription =
    "The Brasilia proposal reduces roll " +
    "size and increases gas limit. Proposer states in their announcement post " +
    "that they have performed extensive testing.\n\n" +
    "Duis suscipit condimentum nisi morbi eros integer ultricies molestie " +
    "etiam morbi rutrum nam tempus, curae velit class hendrerit at sit " +
    "donec volutpat congue enim aenean morbi. Pulvinar metus massa mauris " +
    "fames placerat eros maecenas venenatis rutrum metus laoreet, habitant " +
    "nam arcu praesent taciti eu ante massa dapibus senectus id convallis " +
    "rhoncus nisl pharetra ante eget inceptos mauris est.\n\n" +
    "Potential breaking changes include:\n" +
    "- Inceptos duis venenatis id nostra dui\n" +
    "- massa accumsan taciti massa pulvinar\n" +
    "- tristique justo id libero augue faucibus\n" +
    "- placerat aliquam proin tempus morbi pulvinar suscipit integer " +
    "aptent auctor est.";

  return (
    <Layout>
      <LayoutContent>
        <AgoraHeader />
        <PeriodHeader currentStage="testing" />
        <div className={styles.testing__info}>
          <ProposalDescription
            className={styles.testing__info__description}
            title={proposalDescription.title}
            description={proposalDescription.description}
          />
          <TestingCountdown
            className={styles.testing__info__countdown}
            dateFrom={new Date().toISOString()}
            dateTo={DateTime.local()
              .plus(Duration.fromObject({ day: 4 }))
              .toISO()}
          />
        </div>
      </LayoutContent>
      <div className={styles.bakers__background}>
        <LayoutContent>
          <ProposalDescriptionCard
            className={styles.testing__proposalCard}
            content={proposalDetailedDescription}
          />
        </LayoutContent>
      </div>
    </Layout>
  );
};

export default TestingStagePage;
