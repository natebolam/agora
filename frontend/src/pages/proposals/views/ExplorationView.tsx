import React, { FunctionComponent, ReactElement, useState } from "react";
import { ExplorationPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/ExplorationStagePage.scss";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import BakersFilter from "~/components/proposals/table/BakersFilter";
import BakersTable from "~/components/proposals/table/BakersTable";
import { useTranslation } from "react-i18next";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import { fetchRestBallots } from "~/store/actions/periodActions";
import { PeriodState } from "~/store/reducers/periodReducer";
import { Decision } from "~/models/Decision";
import MajorityGraph from "~/components/proposals/graphs/MajorityGraph";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import NonVotersTable from "~/components/proposals/table/NonVotersTable";
import { Proposer } from "~/models/ProposalInfo";

interface ExplorationViewProps {
  period: ExplorationPeriodInfo;
}

const ExplorationView: FunctionComponent<ExplorationViewProps> = ({
  period,
}): ReactElement => {
  const { t } = useTranslation();
  const [inverted, setInverted] = useState(false);
  const loading: boolean = useSelector(
    (state: RootStoreType): boolean => state.periodStore.ballotsLoading
  );

  const initialBallots: ProposalBallotsList | null = useSelector(
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
  const initialNonVoters: Proposer[] = useSelector(
    (state: RootStoreType): Proposer[] => {
      return state.periodStore.nonVoters || [];
    }
  );

  const initialDecisions = useSelector((state: RootStoreType): Decision[] => {
    return state.periodStore.ballotsDecisions;
  });
  const store = useSelector(
    (state: RootStoreType): PeriodState => state.periodStore
  );

  const [ballots, setBallots] = useState(initialBallots);
  const [nonVoters, setNonVoters] = useState(initialNonVoters.slice(0, 10));
  const [decisions, setDecisions] = useState(initialDecisions);

  const hasMore = (): boolean => {
    if (inverted) return nonVoters.length != initialNonVoters.length;
    return ballots ? ballots.pagination.rest > 0 : false;
  };

  const handleShowAllBallots = (): void => {
    fetchRestBallots(store).then((result): void => {
      if (!result || !ballots) return;
      setBallots({
        pagination: result.payload.pagination,
        results: [...ballots.results, ...result.payload.results],
      });
    });
  };
  const handleShowAllNonVoters = (): void => setNonVoters(initialNonVoters);

  const handleFilterChange = (newValue: Decision[]): void => {
    if (ballots && ballots.pagination.rest) handleShowAllBallots();
    setDecisions(newValue);
  };

  const handleSortChange = (): void => {
    if (ballots && ballots.pagination.rest) handleShowAllBallots();
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
            discourseLink={period.proposal.discourseLink}
            learnMoreLink={`/proposal/${period.proposal.id}`}
          />
          <div className={styles.exploration__voters}>
            <MajorityGraph
              className={styles.exploration__voters__graph}
              ballotsStats={period.ballots}
              voteStats={period.voteStats}
            />
            <ParticipationTracker
              voteStats={period.voteStats}
              onInvert={setInverted}
              hideProgressBar
            />
          </div>
        </div>
      </LayoutContent>
      <LayoutContent className={styles.period__secondaryInfo}>
        {!loading && ballots ? (
          <>
            {inverted ? (
              <NonVotersTable
                data={nonVoters}
                className={styles.bakers__table}
              />
            ) : (
              <>
                <BakersFilter
                  className={styles.bakers__filter}
                  ballots={period.ballots}
                  filter={initialDecisions}
                  onFilterChange={handleFilterChange}
                />
                <BakersTable
                  data={ballots.results.filter(
                    (i): boolean =>
                      !decisions.length || decisions.includes(i.decision)
                  )}
                  className={styles.bakers__table}
                  onSortChange={handleSortChange}
                />
              </>
            )}
            {hasMore() && (
              <button
                className={styles.bakers__showAllButton}
                onClick={
                  inverted ? handleShowAllNonVoters : handleShowAllBallots
                }
              >
                {t("common.showAll")}
              </button>
            )}
          </>
        ) : null}
      </LayoutContent>
    </>
  );
};

export default ExplorationView;
