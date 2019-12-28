import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/StageStage.scss";
import { StageType, StageTimeInfo } from "~/models/Stage";
import AngleIcon from "~/assets/svg/AngleIcon";
import { useTranslation } from "react-i18next";
import { Link } from "react-navi";

interface StageStageIndicatorTypes {
  caption: string;
  isCurrent: boolean;
  link?: string;
}

const ProposalStageIndicator: FunctionComponent<StageStageIndicatorTypes> = ({
  caption,
  isCurrent,
  link,
}): ReactElement => {
  const className = cx(styles.proposalStage__indicator, {
    [styles.proposalStage__indicator_selected]: isCurrent,
    [styles.proposalStage__indicator_disabled]: !link && !isCurrent,
  });
  return link ? (
    <Link className={className} href={link}>
      {caption}
    </Link>
  ) : (
    <div className={className}>{caption}</div>
  );
};

interface StageStageTypes {
  className?: string;
  stage: StageType;
  stageTimes: StageTimeInfo;
  stageId: number;
  hideSelected?: boolean;
}

export const StageStage: FunctionComponent<StageStageTypes> = ({
  className,
  stage,
  stageTimes,
  stageId,
  hideSelected,
}): ReactElement => {
  const stages = ["proposal", "exploration", "testing", "promotion"];

  const idx = (stage: StageType): number =>
    stages.findIndex((s): boolean => s == stage);
  const current = idx(stage);

  const getLink = (
    s1: StageType,
    s2: string,
    i: number
  ): string | undefined => {
    const id = stageId - current + i;
    if (
      (!hideSelected && stage === s1) ||
      !stageTimes[id] ||
      stageTimes[id].stageType != s2
    )
      return;
    return `/stage/${id}`;
  };

  return (
    <div className={className}>
      <div className={styles.proposalStage}>
        <ProposalStageIndicator
          caption="Proposal"
          link={getLink("proposal", "proposal", 0)}
          isCurrent={!hideSelected && stage === "proposal"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Evaluation"
          link={getLink("evaluation", "testing_vote", 1)}
          isCurrent={!hideSelected && stage === "evaluation"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Voting"
          link={getLink("voting", "voting_for_vote", 2)}
          isCurrent={!hideSelected && stage === "voting"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Implementation"
          link={getLink("implementation", "implementation", 3)}
          isCurrent={!hideSelected && stage === "implementation"}
        />
      </div>
    </div>
  );
};

export const StageStageShort: FunctionComponent<StageStageTypes> = ({
  className,
  stage,
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <div className={className}>
      <div className={styles.proposalStage}>
        <ProposalStageIndicator
          caption={t(`stageType.${stage}`)}
          isCurrent={true}
        />
      </div>
    </div>
  );
};
