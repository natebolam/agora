import React, { FunctionComponent, ReactElement, useState } from "react";
import { PromotionPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/PromotionStagePage.scss";
import BakersFilter from "~/components/proposals/table/BakersFilter";
import BakersTable from "~/components/proposals/table/BakersTable";
import { useTranslation } from "react-i18next";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import { useSelector } from "react-redux";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { RootStoreType } from "~/store";
import {
  fetchRestBallots,
  ProposalBallotsSuccessFetchAction,
} from "~/store/actions/periodActions";
import { Decision } from "~/models/Decision";
import MajorityGraph from "~/components/proposals/graphs/MajorityGraph";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";
import NonVotersTable from "~/components/proposals/table/NonVotersTable";

interface PromotionViewProps {
  period: PromotionPeriodInfo;
}

const PromotionView: FunctionComponent<PromotionViewProps> = ({
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

  const initialDecisions = useSelector((state: RootStoreType): Decision[] => {
    return state.periodStore.ballotsDecisions;
  });

  const [ballots, setBallots] = useState(initialBallots);
  const [decisions, setDecisions] = useState(initialDecisions);

  const hasMore = ballots ? ballots.pagination.rest > 0 : false;

  const restBallotsPromise = useSelector(
    (state: RootStoreType): Promise<void | ProposalBallotsSuccessFetchAction> =>
      fetchRestBallots(state)
  );

  const handleShowAll = (): void => {
    restBallotsPromise.then((result): void => {
      if (!result || !ballots) return;
      setBallots({
        pagination: result.payload.pagination,
        results: [...ballots.results, ...result.payload.results],
      });
    });
  };

  const handleFilterChange = (newValue: Decision[]): void => {
    if (ballots && ballots.pagination.rest) handleShowAll();
    setDecisions(newValue);
  };

  const handleSortChange = (): void => {
    if (ballots && ballots.pagination.rest) handleShowAll();
  };

  return (
    <>
      <LayoutContent className={styles.period__primaryInfo}>
        <div>
          <ProposalDescription
            className={styles.promotion__description}
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
          <div className={styles.promotion__voters}>
            <MajorityGraph
              className={styles.promotion__voters__graph}
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
                data={ballots.results}
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
  );
};

export default PromotionView;
