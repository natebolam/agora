import React, {
  FunctionComponent,
  ReactElement,
  useEffect,
  useState,
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
import PeriodHeader from "~/components/proposals/PeriodHeader";
import { Proposal } from "~/models/ProposalInfo";
import { Period, PeriodType, PeriodTimeInfo, VoteStats } from "~/models/Period";
import VotesTable from "~/components/proposals/table/VotesTable";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import {
  fetchRestSpecificProposalVotes,
  SpecificProposalVotesSuccessFetchAction,
} from "~/store/actions/periodActions";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
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
  const period: Period | undefined = useSelector((state: RootStoreType):
    | Period
    | undefined => {
    return state.proposalStore.period;
  });

  const totalPeriods: number = useSelector((state: RootStoreType): number => {
    return state.proposalStore.totalPeriods;
  });

  const periodType: PeriodType | undefined = useSelector(
    (state: RootStoreType): PeriodType | undefined => {
      return state.proposalStore.periodType;
    }
  );
  const periodTimes: PeriodTimeInfo | undefined = useSelector(
    (state: RootStoreType): PeriodTimeInfo | undefined => {
      return state.proposalStore.periodTimes;
    }
  );
  const voteStats: VoteStats | undefined = useSelector((state: RootStoreType):
    | VoteStats
    | undefined => {
    return state.proposalStore.voteStats;
  });
  const winner: Proposal | undefined = useSelector((state: RootStoreType):
    | Proposal
    | undefined => {
    return state.proposalStore.winner;
  });

  const initialSpecificProposalVotes: ProposalVotesList | null = useSelector(
    (state: RootStoreType): ProposalVotesList | null => {
      if (state.periodStore.specificProposalVotes) {
        return {
          pagination: state.periodStore.specificProposalVotes.pagination,
          results: state.periodStore.specificProposalVotes.data,
        };
      }
      return null;
    }
  );

  const [specificProposalVotes, setSpecificProposalVotes] = useState(
    initialSpecificProposalVotes
  );

  const votes = specificProposalVotes || initialSpecificProposalVotes;
  const hasMore = votes ? votes.pagination.rest > 0 : false;

  const restSpecificProposalVotesPromise = useSelector(
    (
      state: RootStoreType
    ): Promise<void | SpecificProposalVotesSuccessFetchAction> =>
      fetchRestSpecificProposalVotes(state)
  );

  const handleShowAll = (): void => {
    restSpecificProposalVotesPromise.then((result): void => {
      const votes = specificProposalVotes || initialSpecificProposalVotes;
      if (!result || !votes) return;
      setSpecificProposalVotes({
        pagination: result.payload.pagination,
        results: [...votes.results, ...result.payload.results],
      });
    });
  };

  const handleSortChange = (): void => {
    const votes = specificProposalVotes || initialSpecificProposalVotes;
    if (votes && votes.pagination.rest) handleShowAll();
  };

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
      <LayoutContent className={styles.periodPage__header}>
        <AgoraHeader />
        {period && periodType && periodTimes ? (
          <PeriodHeader
            currentStage={periodType}
            period={period}
            totalPeriods={totalPeriods}
            periodTimes={periodTimes}
            proposal={winner || null}
            advanced={!!winner}
          />
        ) : null}
      </LayoutContent>
      {proposal ? (
        <>
          <LayoutContent className={styles.period__primaryInfo}>
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
                {voteStats && (
                  <ParticipationTracker
                    className={styles.proposalInfo__votersInfo}
                    voteStats={{
                      ...voteStats,
                      votesCast: proposal.votesCasted,
                      numVoters: proposal.votersNum,
                    }}
                  />
                )}
              </div>
            </div>
          </LayoutContent>
          <LayoutContent className={styles.period__secondaryInfo}>
            <ProposalDescriptionCard
              className={styles.proposalInfo__proposalCard}
              content={
                proposal.longDescription
                  ? proposal.longDescription
                  : t("proposals.common.noDescriptionCaption")
              }
            />
            {specificProposalVotes || initialSpecificProposalVotes ? (
              <>
                <h1 ref={votersRef}>{`${proposal.title} Upvoters`}</h1>
                <VotesTable
                  data={
                    (
                      specificProposalVotes ||
                      initialSpecificProposalVotes || { results: [] }
                    ).results
                  }
                  className={styles.bakers__table}
                  onSortChange={handleSortChange}
                />
                {hasMore && (
                  <button
                    className={styles.bakers__showAllButton}
                    onClick={handleShowAll}
                  >
                    {t("common.showAll")}
                  </button>
                )}
              </>
            ) : null}
          </LayoutContent>
        </>
      ) : null}
    </Layout>
  );
};

export default ProposalInfoPage;
