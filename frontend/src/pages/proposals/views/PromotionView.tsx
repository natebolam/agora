import React, { FunctionComponent, ReactElement } from "react";
import { PromotionPeriodInfo } from "~/models/Period";
import { LayoutContent } from "~/components/common/Layout";
import styles from "~/styles/pages/proposals/PromotionStagePage.scss";
import BakersFilter from "~/components/proposals/BakersFilter";
import BakersTable from "~/components/proposals/BakersTable";
import { useTranslation } from "react-i18next";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import ProposalVoters from "~/components/proposals/ProposalVoters";
import { useDispatch, useSelector } from "react-redux";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { RootStoreType } from "~/store";
import { fetchBallots, fetchMoreBallots } from "~/store/actions/periodActions";
import { Decision } from "~/models/Decision";

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

  const handleShowMore = (): void => {
    dispatch(fetchMoreBallots());
  };

  const currentDecision = useSelector((state: RootStoreType):
    | Decision
    | undefined => {
    return state.periodStore.ballotsDecision;
  });

  const handleFilterChange = (newValue?: Decision): void => {
    dispatch(fetchBallots(period.period.id, newValue));
  };

  return (
    <>
      <LayoutContent className={styles.period__primaryInfo}>
        <div className={styles.promotion__info}>
          <ProposalDescription
            className={styles.exploration__description}
            title={period.proposal.title}
            description={period.proposal.shortDescription}
          />
          <ProposalVoters
            ballotsStats={period.ballots}
            voteStats={period.voteStats}
            className={styles.promotion__voters}
          />
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

export default PromotionView;
