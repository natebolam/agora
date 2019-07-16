import React, { FunctionComponent, ReactElement } from "react";
import { ExplorationPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import styles from "~/styles/pages/proposals/ExplorationStagePage.scss";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import ProposalVoters from "~/components/proposals/ProposalVoters";
import BakersFilter from "~/components/proposals/BakersFilter";
import BakersTable from "~/components/proposals/BakersTable";
import { useTranslation } from "react-i18next";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { useDispatch, useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import { fetchBallots, fetchMoreBallots } from "~/store/actions/periodActions";
import { Decision } from "~/models/Decision";

interface ExplorationViewProps {
  period: ExplorationPeriodInfo;
}

const ExplorationView: FunctionComponent<ExplorationViewProps> = ({
  period,
}): ReactElement => {
  const { t } = useTranslation();
  const dispatch = useDispatch();
  const loading: boolean = useSelector(
    (state: RootStoreType): boolean => state.period.ballotsLoading
  );

  const ballots: ProposalBallotsList | null = useSelector(
    (state: RootStoreType): ProposalBallotsList | null => {
      if (state.period.ballots) {
        return {
          pagination: state.period.ballots.pagination,
          results: state.period.ballots.data,
        };
      }
      return null;
    }
  );

  const hasMore = ballots
    ? ballots.pagination.offset + ballots.pagination.limit <
      ballots.pagination.total
    : false;

  const currentDecision = useSelector((state: RootStoreType):
    | Decision
    | undefined => {
    return state.period.ballotsDecision;
  });

  const handleShowMore = (): void => {
    dispatch(fetchMoreBallots());
  };

  const handleFilterChange = (newValue?: Decision): void => {
    dispatch(fetchBallots(period.period.id, newValue));
  };

  return (
    <>
      <LayoutContent>
        <PeriodHeader
          currentStage="exploration"
          period={period.period}
          totalPeriods={period.totalPeriods}
        />
        <div className={styles.exploration__info}>
          <ProposalDescription
            className={styles.exploration__description}
            title={period.proposal.title}
            description={period.proposal.shortDescription}
          />
          <ProposalVoters
            votesCast={period.voteStats.votesCast}
            votesAvailable={period.voteStats.votesAvailable}
            className={styles.exploration__voters}
          />
        </div>
      </LayoutContent>

      <div className={styles.bakers__background}>
        <LayoutContent>
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
      </div>
    </>
  );
};

export default ExplorationView;
