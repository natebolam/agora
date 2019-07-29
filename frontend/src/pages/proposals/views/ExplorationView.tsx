import React, { FunctionComponent, ReactElement } from "react";
import { ExplorationPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/ExplorationStagePage.scss";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import BakersFilter from "~/components/proposals/BakersFilter";
import BakersTable from "~/components/proposals/BakersTable";
import { useTranslation } from "react-i18next";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { useDispatch, useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import { fetchBallots, fetchMoreBallots } from "~/store/actions/periodActions";
import { Decision } from "~/models/Decision";
import MajorityGraph from "~/components/proposals/graphs/MajorityGraph";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";

interface ExplorationViewProps {
  period: ExplorationPeriodInfo;
}

const ExplorationView: FunctionComponent<ExplorationViewProps> = ({
  period,
}): ReactElement => {
  const { t } = useTranslation();
  const dispatch = useDispatch();
  const loading: boolean = useSelector(
    (state: RootStoreType): boolean => state.periodStore.ballotsLoading
  );

  const ballots: ProposalBallotsList | null = useSelector(
    (state: RootStoreType): ProposalBallotsList | null => {
      if (state.periodStore.ballots) {
        return {
          pagination: state.periodStore.ballots.pagination,
          results: state.periodStore.ballots.data,
        };
      }
      return null;
    }
  );

  const hasMore = ballots ? ballots.pagination.rest > 0 : false;

  const currentDecision = useSelector((state: RootStoreType):
    | Decision
    | undefined => {
    return state.periodStore.ballotsDecision;
  });

  const handleShowMore = (): void => {
    dispatch(fetchMoreBallots());
  };

  const handleFilterChange = (newValue?: Decision): void => {
    dispatch(fetchBallots(period.period.id, newValue));
  };

  return (
    <>
      <LayoutContent className={styles.period__primaryInfo}>
        <div>
          <ProposalDescription
            className={styles.exploration__description}
            title={
              period.proposal.title
                ? period.proposal.title
                : period.proposal.hash
            }
            description={
              period.proposal.shortDescription
                ? period.proposal.shortDescription
                : t("proposals.common.noDescriptionCaption")
            }
          />
          <div className={styles.exploration__voters}>
            <MajorityGraph
              className={styles.exploration__voters__graph}
              ballotsStats={period.ballots}
              voteStats={period.voteStats}
            />
            <ParticipationTracker
              className={styles.explorationVoters__tracker}
              voteStats={period.voteStats}
              hideProgressBar
            />
          </div>
        </div>
      </LayoutContent>
      <LayoutContent className={styles.period__secondaryInfo}>
        <BakersFilter
          className={styles.bakers__filter}
          ballots={period.ballots}
          filter={currentDecision}
          onFilterChange={handleFilterChange}
        />
        {!loading && ballots ? (
          <>
            <BakersTable
              data={ballots.results}
              className={styles.bakers__table}
            />
            {hasMore && (
              <button
                className={styles.bakers__showMoreButton}
                onClick={handleShowMore}
              >
                {t("common.showMore")}
              </button>
            )}
          </>
        ) : null}
      </LayoutContent>
    </>
  );
};

export default ExplorationView;
