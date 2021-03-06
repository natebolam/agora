import React, { FunctionComponent, ReactElement, useState } from "react";
import cx from "classnames";
import Card from "~/components/common/Card";
import * as styles from "~/styles/components/proposals/ProposalsList.scss";
import {
  ProposalsList as ProposalsListType,
  Proposal as ProposalsListItem,
} from "~/models/ProposalInfo";
import ChatIcon from "~/assets/svg/ChatIcon";
import InfoIcon from "~/assets/svg/InfoIcon";
import { useTranslation } from "react-i18next";
import { Link } from "react-navi";

interface ProposalsListItemTypes {
  proposal: ProposalsListItem;
}

const ProposalListItem: FunctionComponent<ProposalsListItemTypes> = ({
  proposal,
}): ReactElement => {
  const discourseLink = proposal.discourseLink ? proposal.discourseLink : "#";
  const { t } = useTranslation();
  const [changedProposal, changeProposal] = useState(false);

  return (
    <Card className={styles.list__item}>
      <div className={styles.list__item__info}>
        <div className={styles.list__item__main}>
          <div
            className={styles.list__item__title}
            onClick={(): void => changeProposal(!changedProposal)}
          >
            {!proposal.title || changedProposal
              ? proposal.hash
              : proposal.title}
          </div>
          <div
            className={styles.list__item__description}
            dangerouslySetInnerHTML={{
              __html: proposal.shortDescription
                ? proposal.shortDescription
                : t("proposals.common.noShortDescriptionCaption"),
            }}
          />
          <div className={styles.list__item__buttons}>
            <Link href={`/proposal/${proposal.stage}/${proposal.id}`}>
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
