import React, { FunctionComponent, ReactElement } from "react";
import { PromotionPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/PromotionStagePage.scss";
import BakersFilter from "~/components/proposals/table/BakersFilter";
import BakersTable from "~/components/proposals/table/BakersTable";
import { useTranslation } from "react-i18next";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import { useDispatch, useSelector } from "react-redux";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { RootStoreType } from "~/store";
import {
  fetchBallots,
  fetchRestBallots,
  ProposalBallotsSuccessFetchAction,
} from "~/store/actions/periodActions";
import { Decision } from "~/models/Decision";
import MajorityGraph from "~/components/proposals/graphs/MajorityGraph";
import ParticipationTracker from "~/components/proposals/ParticipationTracker";

interface PromotionViewProps {
  period: PromotionPeriodInfo;
}

const PromotionView: FunctionComponent<PromotionViewProps> = ({
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

  const restBallotsPromise = useSelector(
    (state: RootStoreType): Promise<void | ProposalBallotsSuccessFetchAction> =>
      fetchRestBallots(state)
  );

  const handleShowAll = (): void => {
    restBallotsPromise.then((result): void => {
      if (result) dispatch(result);
    });
  };

  const currentDecision = useSelector((state: RootStoreType): Decision[] => {
    return state.periodStore.ballotsDecisions;
  });

  const handleFilterChange = (newValue: Decision[]): void => {
    dispatch(fetchBallots(period.period.id, newValue));
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
          />
          <div className={styles.promotion__voters}>
            <MajorityGraph
              className={styles.promotion__voters__graph}
              ballotsStats={period.ballots}
              voteStats={period.voteStats}
            />
            <ParticipationTracker
              voteStats={period.voteStats}
              hideProgressBar
            />
          </div>
        </div>
      </LayoutContent>
      <LayoutContent className={styles.period__secondaryInfo}>
        {!loading && ballots ? (
          <>
            <BakersFilter
              className={styles.bakers__filter}
              ballots={period.ballots}
              filter={currentDecision}
              onFilterChange={handleFilterChange}
            />
            <BakersTable
              data={ballots.results}
              className={styles.bakers__table}
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
  );
};

export default PromotionView;
