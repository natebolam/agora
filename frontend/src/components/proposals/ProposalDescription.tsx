import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import DiscourseButton from "~/components/controls/DiscourseButton";
import styles from "~/styles/components/proposals/ProposalDescription.scss";

interface ExplorationInfoTypes {
  className?: string;
  title: string;
  description: string;
}

const ProposalDescription: FunctionComponent<ExplorationInfoTypes> = ({
  title,
  description,
  className,
}): ReactElement => {
  return (
    <div className={cx(className, styles.explorationDescription)}>
      <h1 className={styles.explorationDescription__title}>{title}</h1>
      <div className={styles.explorationDescription__description}>
        {description}
      </div>
      <DiscourseButton
        className={styles.explorationDescription__discussButton}
      />
    </div>
  );
};

export default ProposalDescription;
