import React, { FunctionComponent, ReactElement } from "react";
import cx from "classnames";
import Card from "~/components/common/Card";
import * as styles from "~/styles/components/proposals/ProposalsList.scss";
import {
  ProposalsList as ProposalsListType,
  ProposalsListItem,
} from "~/models/ProposalsList";
import { Link } from "react-router-dom";
import ChatIcon from "~/assets/svg/ChatIcon";
import InfoIcon from "~/assets/svg/InfoIcon";
import ProposalVotesGraph from "~/components/proposals/ProposalVotesGraph";
import { useTranslation } from "react-i18next";

interface ProposalsListItemTypes {
  proposal: ProposalsListItem;
}

const ProposalListItem: FunctionComponent<ProposalsListItemTypes> = ({
  proposal,
}): ReactElement => {
  const discourseLink = proposal.discourseLink ? proposal.discourseLink : "#";
  const votes = [0, 10, 1, 12, 41, 2, 20];
  const { t } = useTranslation();
  return (
    <Card className={styles.list__item}>
      <div className={styles.list__item__info}>
        <div className={styles.list__item__upvotes}>
          <div className={styles.list__item__upvotes__title}>
            {t("proposals.proposalsList.upvotesCaption")}
          </div>
          <ProposalVotesGraph
            className={styles.list__item__upvotes__grpah}
            votes={votes}
          />
          <div className={styles.list__item__upvotes__value}>
            {t("proposals.proposalsList.upvotesValue", {
              value: proposal.votesCasted,
            })}
          </div>
        </div>
        <div className={styles.list__item__main}>
          <div className={styles.list__item__name}>
            {proposal.proposer.name}
          </div>
          <div className={styles.list__item__title}>{proposal.title}</div>
          <div className={styles.list__item__description}>
            {proposal.shortDescription}
          </div>
          <div className={styles.list__item__buttons}>
            <Link to={`/proposal/${proposal.id}`}>
              <InfoIcon />
              {t("proposals.proposalsList.learnMore")}
            </Link>
            <a href={discourseLink}>
              <ChatIcon />
              {t("proposals.proposalsList.discuss")}
            </a>
          </div>
        </div>
      </div>
    </Card>
  );
};

interface ProposalsListTypes {
  className?: string;
  proposals: ProposalsListType;
}

const ProposalsList: FunctionComponent<ProposalsListTypes> = ({
  className,
  proposals,
}): ReactElement => {
  console.log(proposals);
  return (
    <div className={cx(className, styles.list)}>
      {proposals.map(
        (proposal, index): ReactElement => (
          <ProposalListItem proposal={proposal} key={index} />
        )
      )}
    </div>
  );
};

export default ProposalsList;
