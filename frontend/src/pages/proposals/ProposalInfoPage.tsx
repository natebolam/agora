import React, { FunctionComponent, ReactElement, useEffect } from "react";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import useRouter from "use-react-router";
import { useDispatch, useSelector } from "react-redux";
import { RootStoreType } from "~/store";
import ProposalStore from "~/store/actions/proposalActions";
import ProposalDescription from "~/components/proposals/ProposalDescription";
import ProposalDescriptionCard from "~/components/proposals/ProposalDescriptionCard";
import { useTranslation } from "react-i18next";
import ProposalDetails from "~/components/proposals/ProposalDetails";
import styles from "~/styles/pages/proposals/ProposalInfoPage.scss";
import PeriodHeader from "~/components/proposals/PeriodHeader";
import { Proposal } from "~/models/ProposalInfo";
import { Period, PeriodType } from "~/models/Period";

interface ProposalInfoPageParams {
  id: number;
}

const ProposalInfoPage: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();
  const { match, history } = useRouter();
  const dispatch = useDispatch();

  const id = (match.params as ProposalInfoPageParams).id;

  useEffect((): void => {
    dispatch(ProposalStore.actionCreators.fetchProposal(id));
  }, [id]);

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

  const loading: boolean = useSelector((state: RootStoreType): boolean => {
    return state.proposalStore.isLoading;
  });

  const errorCode: number | null = useSelector((state: RootStoreType):
    | number
    | null => {
    return state.proposalStore.error
      ? state.proposalStore.error.errorCode
      : null;
  });

  useEffect((): void => {
    if (errorCode) {
      history.replace(`/error/${errorCode}`);
    }
  }, [errorCode]);

  return (
    <Layout>
      <LayoutContent className={styles.periodPage__header}>
        <AgoraHeader />
        {!loading && period && periodType ? (
          <PeriodHeader
            currentStage={periodType}
            period={period}
            totalPeriods={totalPeriods}
          />
        ) : null}
      </LayoutContent>
      {!loading && proposal ? (
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
              <ProposalDetails
                className={styles.proposalInfo__details}
                proposal={proposal}
              />
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
          </LayoutContent>
        </>
      ) : null}
    </Layout>
  );
};

export default ProposalInfoPage;
