import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/PeriodStage.scss";
import { PeriodType, PeriodTimeInfo } from "~/models/Period";
import AngleIcon from "~/assets/svg/AngleIcon";
import { useTranslation } from "react-i18next";
import { Link } from "react-navi";

interface PeriodStageIndicatorTypes {
  caption: string;
  isCurrent: boolean;
  link?: string;
}

const ProposalStageIndicator: FunctionComponent<PeriodStageIndicatorTypes> = ({
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

interface PeriodStageTypes {
  className?: string;
  stage: PeriodType;
  periodTimes: PeriodTimeInfo;
  periodId: number;
  isProposal?: boolean;
}

export const PeriodStage: FunctionComponent<PeriodStageTypes> = ({
  className,
  stage,
  periodTimes,
  periodId,
  isProposal,
}): ReactElement => {
  const stages = ["proposal", "exploration", "testing", "promotion"];

  const idx = (stage: PeriodType): number =>
    stages.findIndex((s): boolean => s == stage);
  const current = idx(stage);

  const getLink = (
    s1: PeriodType,
    s2: string,
    i: number
  ): string | undefined => {
    const id = periodId - current + i;
    if (
      (!isProposal && stage === s1) ||
      !periodTimes[id] ||
      periodTimes[id].periodType != s2
    )
      return;
    return `/period/${id}`;
  };

  return (
    <div className={className}>
      <div className={styles.proposalStage}>
        <ProposalStageIndicator
          caption="Proposal"
          link={getLink("proposal", "proposal", 0)}
          isCurrent={!isProposal && stage === "proposal"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Exploration"
          link={getLink("exploration", "testing_vote", 1)}
          isCurrent={stage === "exploration"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Testing"
          link={getLink("testing", "testing", 2)}
          isCurrent={stage === "testing"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Promotion"
          link={getLink("promotion", "promotion_vote", 3)}
          isCurrent={stage === "promotion"}
        />
      </div>
    </div>
  );
};

export const PeriodStageShort: FunctionComponent<PeriodStageTypes> = ({
  className,
  stage,
}): ReactElement => {
  const { t } = useTranslation();

  return (
    <div className={className}>
      <div className={styles.proposalStage}>
        <ProposalStageIndicator
          caption={t(`periodType.${stage}`)}
          isCurrent={true}
        />
      </div>
    </div>
  );
};
