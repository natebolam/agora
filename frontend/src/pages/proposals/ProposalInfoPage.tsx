import React, {
  FunctionComponent,
  ReactElement,
  useEffect,
  createRef,
  RefObject,
} from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import { useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import ProposalDescriptionCard from "~/components/proposals/ProposalDescriptionCard";
import { useTranslation } from "react-i18next";
import ProposalDetails from "~/components/proposals/ProposalDetails";
import styles from "~/styles/pages/proposals/ProposalInfoPage.scss";
import StageHeader from "~/components/proposals/StageHeader";
import { Proposal } from "~/models/ProposalInfo";
import { StageType, StageTimeInfo } from "~/models/Stage";
import VotesTable from "~/components/proposals/table/VotesTable";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import BusyIndicator from "react-busy-indicator";
import { useLoadingRoute } from "react-navi";

const ProposalInfoPage: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();

  const votersRef: RefObject<HTMLHeadingElement> = createRef();

  const proposal: Proposal | undefined = useSelector((state: RootStoreType):
    | Proposal
    | undefined => {
    return state.proposalStore.proposal;
  });
  const stage: number | undefined = useSelector((state: RootStoreType):
    | number
    | undefined => {
    return state.proposalStore.stage;
  });

  const totalStages: number = useSelector((state: RootStoreType): number => {
    return state.proposalStore.totalStages;
  });

  const stageType: StageType | undefined = useSelector((state: RootStoreType):
    | StageType
    | undefined => {
    return state.proposalStore.stageType;
  });
  const stageTimes: StageTimeInfo | undefined = useSelector(
    (state: RootStoreType): StageTimeInfo | undefined => {
      return state.proposalStore.stageTimes;
    }
  );
  const winner: Proposal | undefined = useSelector((state: RootStoreType):
    | Proposal
    | undefined => {
    return state.proposalStore.winner;
  });

  const initialSpecificProposalVotes: ProposalVotesList | null = useSelector(
    (state: RootStoreType): ProposalVotesList | null => {
      if (state.stageStore.specificProposalVotes) {
        return state.stageStore.specificProposalVotes;
      }
      return null;
    }
  );

  useEffect((): void => {
    if (location.hash == "#voters" && votersRef.current) {
      votersRef.current.scrollIntoView({
        behavior: "smooth",
        block: "start",
      });
    }
  });

  const loadingRoute = useLoadingRoute();

  return (
    <Layout>
      <BusyIndicator
        active={!!loadingRoute}
        delayMs={0}
        className={""}
        color={"blue"}
        isBusy={!!loadingRoute}
        style={{}}
      />
      <LayoutContent className={styles.stagePage__header}>
        <AgoraHeader />
        {stage && stageType && stageTimes ? (
          <StageHeader
            currentStage={stageType}
            stage={stage}
            totalStages={totalStages}
            stageTimes={stageTimes}
            proposal={winner || null}
            hideSelected={true}
          />
        ) : null}
      </LayoutContent>
      {proposal && (
        <>
          <LayoutContent className={styles.stage__primaryInfo}>
            <div>
              <ProposalDescription
                className={styles.proposalInfo__description}
                title={proposal.title ? proposal.title : proposal.hash}
                description={
                  proposal.shortDescription
                    ? proposal.shortDescription
                    : t("proposals.common.noDescriptionCaption")
                }
                discourseLink={proposal.discourseLink}
              />
              <div>
                <ProposalDetails
                  className={styles.proposalInfo__details}
                  proposal={proposal}
                />
              </div>
            </div>
          </LayoutContent>
          <LayoutContent className={styles.stage__secondaryInfo}>
            <ProposalDescriptionCard
              className={styles.proposalInfo__proposalCard}
              content={
                proposal.longDescription
                  ? proposal.longDescription
                  : t("proposals.common.noDescriptionCaption")
              }
            />
            {initialSpecificProposalVotes &&
              initialSpecificProposalVotes.length !== 0 && (
                <>
                  <h1 ref={votersRef}>{`${proposal.title} Upvoters`}</h1>
                  <VotesTable
                    data={initialSpecificProposalVotes}
                    className={styles.voters__table}
                  />
                </>
              )}
          </LayoutContent>
        </>
      )}
    </Layout>
  );
};

export default ProposalInfoPage;
