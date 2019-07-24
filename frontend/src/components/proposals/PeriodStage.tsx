import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import styles from "~/styles/components/proposals/PeriodStage.scss";
import { ProposalType } from "~/models/Period";
import AngleIcon from "~/assets/svg/AngleIcon";
import { useTranslation } from "react-i18next";

interface PeriodStageIndicatorTypes {
  caption: string;
  isCurrent: boolean;
}

const ProposalStageIndicator: FunctionComponent<PeriodStageIndicatorTypes> = ({
  caption,
  isCurrent,
}): ReactElement => {
  return (
    <div
      className={cx(styles.proposalStage__indicator, {
        [styles.proposalStage__indicator_selected]: isCurrent,
      })}
    >
      {caption}
    </div>
  );
};

interface PeriodStageTypes {
  className?: string;
  stage: ProposalType;
}

export const PeriodStage: FunctionComponent<PeriodStageTypes> = ({
  className,
  stage,
}): ReactElement => {
  return (
    <div className={className}>
      <div className={styles.proposalStage}>
        <ProposalStageIndicator
          caption="Proposal"
          isCurrent={stage === "proposal"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Exploration"
          isCurrent={stage === "exploration"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Testing"
          isCurrent={stage === "testing"}
        />
        <AngleIcon />
        <ProposalStageIndicator
          caption="Promotion"
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
