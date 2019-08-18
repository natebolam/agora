import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import DiscourseButton from "~/components/controls/DiscourseButton";
import styles from "~/styles/components/proposals/ProposalDescription.scss";
import LearnMoreButton from "../controls/LearnMoreButton";

interface ExplorationInfoTypes {
  className?: string;
  title: string;
  description: string;
  discourseLink: string;
  learnMoreLink?: string;
}

const ProposalDescription: FunctionComponent<ExplorationInfoTypes> = ({
  title,
  description,
  discourseLink,
  learnMoreLink,
  className,
}): ReactElement => {
  return (
    <div className={cx(className, styles.explorationDescription)}>
      <h1 className={styles.explorationDescription__title}>{title}</h1>
      <div
        className={styles.explorationDescription__description}
        dangerouslySetInnerHTML={{ __html: description }}
      />
      <div className={styles.explorationDescription__buttons}>
        {learnMoreLink && (
          <LearnMoreButton
            className={styles.explorationDescription__moreButton}
            href={learnMoreLink}
          />
        )}
        <DiscourseButton
          className={styles.explorationDescription__discussButton}
          href={discourseLink}
        />
      </div>
    </div>
  );
};

export default ProposalDescription;
