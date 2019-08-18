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
import { useTranslation } from "react-i18next";

interface ProposalsListItemTypes {
  proposal: ProposalsListItem;
}

const ProposalListItem: FunctionComponent<ProposalsListItemTypes> = ({
  proposal,
}): ReactElement => {
  const discourseLink = proposal.discourseLink ? proposal.discourseLink : "#";
  const { t } = useTranslation();
  return (
    <Card className={styles.list__item}>
      <div className={styles.list__item__info}>
        <div className={styles.list__item__upvotes}>
          <a
            className={styles.list__item__upvotes__title}
            href={`/proposal/${proposal.id}#voters`}
          >
            {t("proposals.proposalsList.upvotesCaption")}
          </a>
          <div className={styles.list__item__upvotes__value}>
            {t("proposals.proposalsList.upvotesValue", {
              value: proposal.votesCasted,
            })}
          </div>
        </div>
        <div className={styles.list__item__main}>
          <div className={styles.list__item__name}>
            {proposal.proposer.name
              ? proposal.proposer.name
              : proposal.proposer.pkh}
          </div>
          <div className={styles.list__item__title}>
            {proposal.title ? proposal.title : proposal.hash}
          </div>
          <div
            className={styles.list__item__description}
            dangerouslySetInnerHTML={{
              __html: proposal.shortDescription
                ? proposal.shortDescription
                : t("proposals.proposalsList.noDescriptionCaption"),
            }}
          />
          <div className={styles.list__item__buttons}>
            <Link to={`/proposal/${proposal.id}`} target="_blank">
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
