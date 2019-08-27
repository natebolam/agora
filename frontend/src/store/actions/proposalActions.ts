import { ThunkAction } from "redux-thunk";
import { RootStoreType } from "~/store";
import { Action } from "redux";
import { Api } from "~/api/api";
import { Proposal } from "~/models/ProposalInfo";
import {
  Period,
  PeriodType,
  PeriodTimeInfo,
  ProposalPeriodInfo,
  VoteStats,
} from "~/models/Period";
import { fetchSpecificProposalVotes } from "./periodActions";

const PROPOSAL_START_FETCH = "@@proposal/start_fetch";
const PROPOSAL_SUCCESS_FETCH = "@@proposal/success_fetch";
const PROPOSAL_ERROR_FETCH = "@@proposal/error_fetch";

const actions = {
  PROPOSAL_START_FETCH,
  PROPOSAL_SUCCESS_FETCH,
  PROPOSAL_ERROR_FETCH,
};

export interface ProposalStartFetchAction {
  type: typeof PROPOSAL_START_FETCH;
}

export interface ProposalSuccessFetchAction {
  type: typeof PROPOSAL_SUCCESS_FETCH;
  payload: {
    proposal: Proposal;
    period: Period;
    totalPeriods: number;
    periodType: PeriodType;
    periodTimes: PeriodTimeInfo;
    voteStats: VoteStats;
    winner: Proposal;
    advanced: boolean;
  };
}

export interface ProposalErrorFetchAction {
  type: typeof PROPOSAL_ERROR_FETCH;
  payload: {
    errorCode: number;
    errorMessage: string;
  };
}

export type ProposalActionTypes =
  | ProposalStartFetchAction
  | ProposalSuccessFetchAction
  | ProposalErrorFetchAction;

const proposalStartFetchAction = (): ProposalStartFetchAction => {
  return {
    type: PROPOSAL_START_FETCH,
  };
};
const proposalSuccessFetchAction = (
  proposal: Proposal,
  period: Period,
  totalPeriods: number,
  periodType: PeriodType,
  periodTimes: PeriodTimeInfo,
  voteStats: VoteStats,
  winner: Proposal,
  advanced: boolean
): ProposalSuccessFetchAction => {
  return {
    type: PROPOSAL_SUCCESS_FETCH,
    payload: {
      proposal,
      period,
      totalPeriods,
      periodType,
      periodTimes,
      voteStats,
      winner,
      advanced,
    },
  };
};

const proposalErrorFetchAction = (
  errorCode: number,
  errorMessage: string
): ProposalErrorFetchAction => {
  return {
    type: PROPOSAL_ERROR_FETCH,
    payload: {
      errorCode,
      errorMessage,
    },
  };
};

const fetchProposal = (
  proposalId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalStartFetchAction());
    try {
      const proposal = await Api.agoraApi.getProposal(proposalId);
      const period = await Api.agoraApi.getPeriod(proposal.period);
      await dispatch(await fetchSpecificProposalVotes(proposalId));
      dispatch(
        proposalSuccessFetchAction(
          proposal,
          period.period,
          period.totalPeriods,
          period.type,
          period.periodTimes,
          (period as ProposalPeriodInfo).voteStats,
          (period as ProposalPeriodInfo).winner,
          period.advanced
        )
      );
    } catch (e) {
      if (e.response) {
        dispatch(
          proposalErrorFetchAction(e.response.status, e.response.statusText)
        );
      } else {
        dispatch(proposalErrorFetchAction(404, ""));
      }
    }
  };
};

const actionCreators = {
  fetchProposal,
};

const ProposalStore = {
  actions,
  actionCreators,
};

export default ProposalStore;
