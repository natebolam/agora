import React, { FunctionComponent, ReactElement } from "react";
import DiscourseButton from "~/components/controls/DiscourseButton";
import ChartMock from "~/assets/png/external_icon.png";
import styles from "~/styles/components/proposals/ProposalDescription.scss";

interface ExplorationInfoTypes {
  className?: string;
  title: string;
  description: string;
}

const ProposalDescription: FunctionComponent<ExplorationInfoTypes>
  = ({ title, description, className }): ReactElement => {
  return (
    <div className={className}>
      <h1 className={styles.explorationDescription__title}>
        {title}
      </h1>
      <div className={styles.explorationDescription__description}>
        {description}
      </div>
      <DiscourseButton className={styles.explorationDescription__discussButton}/>
    </div>
  );
};

export default ProposalDescription;
