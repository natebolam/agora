import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import DiscourseButton from "~/components/controls/DiscourseButton";
import styles from "~/styles/components/proposals/ProposalDescription.scss";

interface ExplorationInfoTypes {
  className?: string;
  title: string;
  description: string;
  discourseLink: string;
}

const ProposalDescription: FunctionComponent<ExplorationInfoTypes> = ({
  title,
  description,
  discourseLink,
  className,
}): ReactElement => {
  return (
    <div className={cx(className, styles.explorationDescription)}>
      <h1 className={styles.explorationDescription__title}>{title}</h1>
      <div
        className={styles.explorationDescription__description}
        dangerouslySetInnerHTML={{ __html: description }}
      />
      <DiscourseButton
        className={styles.explorationDescription__discussButton}
        href={discourseLink}
      />
    </div>
  );
};

export default ProposalDescription;
