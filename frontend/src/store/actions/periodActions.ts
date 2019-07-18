import { MetaPeriodInfo } from "~/models/Period";
import { ThunkAction } from "redux-thunk";
import { RootStoreType } from "~/store";
import { Action } from "redux";
import { Api } from "~/api/api";
import { ProposalsList } from "~/models/ProposalsList";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { Decision } from "~/models/Decision";

const PERIOD_START_FETCH = "@@period/start_fetch";
const PERIOD_SUCCESS_FETCH = "@@period/success_fetch";
const PERIOD_ERROR_FETCH = "@@period/error_fetch";

const PROPOSALS_START_FETCH = "@@period/proposals/start_fetch";
const PROPOSALS_SUCCESS_FETCH = "@@period/proposals/success_fetch";
const PROPOSALS_ERROR_FETCH = "@@period/proposals/error_fetch";

const PROPOSAL_VOTES_START_FETCH = "@@period/proposal_votes/start_fetch";
const PROPOSAL_VOTES_SUCCESS_FETCH = "@@period/proposal_votes/success_fetch";
const PROPOSAL_VOTES_ERROR_FETCH = "@@period/proposal_votes/error_fetch";

const PROPOSAL_BALLOTS_START_FETCH = "@@period/proposal_ballots/start_fetch";
const PROPOSAL_BALLOTS_SUCCESS_FETCH =
  "@@period/proposal_ballots/success_fetch";
const PROPOSAL_BALLOTS_ERROR_FETCH = "@@period/proposal_ballots/error_fetch";

const actions = {
  PERIOD_START_FETCH,
  PERIOD_SUCCESS_FETCH,
  PERIOD_ERROR_FETCH,
  PROPOSALS_START_FETCH,
  PROPOSALS_SUCCESS_FETCH,
  PROPOSALS_ERROR_FETCH,
  PROPOSAL_VOTES_START_FETCH,
  PROPOSAL_VOTES_SUCCESS_FETCH,
  PROPOSAL_VOTES_ERROR_FETCH,
  PROPOSAL_BALLOTS_START_FETCH,
  PROPOSAL_BALLOTS_SUCCESS_FETCH,
  PROPOSAL_BALLOTS_ERROR_FETCH,
};

export interface PeriodStartFetchAction {
  type: typeof PERIOD_START_FETCH;
}

export interface PeriodSuccessFetchAction {
  type: typeof PERIOD_SUCCESS_FETCH;
  payload: {
    result: MetaPeriodInfo;
  };
}

export interface PeriodErrorFetchAction {
  type: typeof PERIOD_ERROR_FETCH;
  payload: {
    errorCode: number;
    errorMessage: string;
  };
}

export interface ProposalsStartFetchAction {
  type: typeof PROPOSALS_START_FETCH;
}

export interface ProposalsSuccessFetchAction {
  type: typeof PROPOSALS_SUCCESS_FETCH;
  payload: ProposalsList;
  isLoadMore: boolean;
}

export interface ProposalsErrorFetchAction {
  type: typeof PROPOSALS_ERROR_FETCH;
  payload: {
    errorCode: number;
    errorMessage: string;
  };
}

export interface ProposalVotesStartFetchAction {
  type: typeof PROPOSAL_VOTES_START_FETCH;
}

export interface ProposalVotesSuccessFetchAction {
  type: typeof PROPOSAL_VOTES_SUCCESS_FETCH;
  payload: ProposalVotesList;
  isLoadMore: boolean;
}

export interface ProposalVotesErrorFetchAction {
  type: typeof PROPOSAL_VOTES_ERROR_FETCH;
  payload: {
    errorCode: number;
    errorMessage: string;
  };
}

export interface ProposalBallotsStartFetchAction {
  type: typeof PROPOSAL_BALLOTS_START_FETCH;
}

export interface ProposalBallotsSuccessFetchAction {
  type: typeof PROPOSAL_BALLOTS_SUCCESS_FETCH;
  payload: ProposalBallotsList;
  isLoadMore: boolean;
  decision?: Decision;
}

export interface ProposalBallotsErrorFetchAction {
  type: typeof PROPOSAL_BALLOTS_ERROR_FETCH;
  payload: {
    errorCode: number;
    errorMessage: string;
  };
}

export type PeriodActionTypes =
  | PeriodStartFetchAction
  | PeriodSuccessFetchAction
  | PeriodErrorFetchAction
  | ProposalsStartFetchAction
  | ProposalsSuccessFetchAction
  | ProposalsErrorFetchAction
  | ProposalVotesStartFetchAction
  | ProposalVotesSuccessFetchAction
  | ProposalVotesErrorFetchAction
  | ProposalBallotsStartFetchAction
  | ProposalBallotsSuccessFetchAction
  | ProposalBallotsErrorFetchAction;

const periodStartFetchAction = (): PeriodStartFetchAction => {
  return {
    type: PERIOD_START_FETCH,
  };
};

const periodSuccessFetchAction = (
  result: MetaPeriodInfo
): PeriodSuccessFetchAction => {
  return {
    type: PERIOD_SUCCESS_FETCH,
    payload: {
      result,
    },
  };
};

const proposalsStartFetchAction = (): ProposalsStartFetchAction => {
  return {
    type: PROPOSALS_START_FETCH,
  };
};

const proposalsSuccessFetchAction = (
  result: ProposalsList,
  isLoadMore: boolean = false
): ProposalsSuccessFetchAction => {
  return {
    type: PROPOSALS_SUCCESS_FETCH,
    payload: result,
    isLoadMore,
  };
};

const proposalVotesStartFetchAction = (): ProposalVotesStartFetchAction => {
  return {
    type: PROPOSAL_VOTES_START_FETCH,
  };
};

const proposalVotesSuccessFetchAction = (
  result: ProposalVotesList,
  isLoadMore: boolean = false
): ProposalVotesSuccessFetchAction => {
  return {
    type: PROPOSAL_VOTES_SUCCESS_FETCH,
    payload: result,
    isLoadMore,
  };
};

const proposalBallotsSuccessFetchAction = (
  result: ProposalBallotsList,
  decision?: Decision,
  isLoadMore: boolean = false
): ProposalBallotsSuccessFetchAction => {
  return {
    type: PROPOSAL_BALLOTS_SUCCESS_FETCH,
    payload: result,
    isLoadMore,
    decision,
  };
};

export const fetchProposals = (
  periodId: number,
  lastId?: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalsStartFetchAction());
    const result = await Api.agoraApi.getProposals(periodId, lastId);
    dispatch(proposalsSuccessFetchAction(result, lastId !== undefined));
  };
};

export const fetchMoreProposals = (): ThunkAction<
  void,
  RootStoreType,
  null,
  Action
> => {
  return async (dispatch, getState): Promise<void> => {
    const periodInfo = getState().periodStore.period;
    const proposals = getState().periodStore.proposals;
    if (periodInfo && periodInfo.period && proposals) {
      const result = await Api.agoraApi.getProposals(
        periodInfo.period.id,
        proposals.pagination.lastId
      );
      dispatch(proposalsSuccessFetchAction(result, true));
    }
  };
};

export const fetchProposalVotes = (
  periodId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalVotesStartFetchAction());
    const result = await Api.agoraApi.getProposalVotes(periodId);
    dispatch(proposalVotesSuccessFetchAction(result));
  };
};

export const fetchMoreProposalVotes = (): ThunkAction<
  void,
  RootStoreType,
  null,
  Action
> => {
  return async (dispatch, getState): Promise<void> => {
    const periodInfo = getState().periodStore.period;
    const proposalVotes = getState().periodStore.proposalVotes;
    if (periodInfo && periodInfo.period && proposalVotes) {
      const result = await Api.agoraApi.getProposalVotes(
        periodInfo.period.id,
        proposalVotes.pagination.lastId
      );
      dispatch(proposalVotesSuccessFetchAction(result, true));
    }
  };
};

export const fetchBallots = (
  periodId: number,
  decision?: Decision
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    const result = await Api.agoraApi.getBallots(periodId, decision);
    dispatch(proposalBallotsSuccessFetchAction(result, decision));
  };
};

export const fetchMoreBallots = (): ThunkAction<
  void,
  RootStoreType,
  null,
  Action
> => {
  return async (dispatch, getState): Promise<void> => {
    const periodInfo = getState().periodStore.period;
    const ballots = getState().periodStore.ballots;
    const ballotsDecision = getState().periodStore.ballotsDecision;

    if (periodInfo && periodInfo.period && ballots) {
      console.log("Current decision: ", ballotsDecision);
      const result = await Api.agoraApi.getBallots(
        periodInfo.period.id,
        ballotsDecision ? ballotsDecision : undefined,
        ballots.pagination.lastId
      );
      dispatch(
        proposalBallotsSuccessFetchAction(result, ballotsDecision, true)
      );
    }
  };
};

const fetchWelcomePage = (): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(periodStartFetchAction());
    const result = await Api.agoraApi.getPeriod();
    dispatch(periodSuccessFetchAction(result));
  };
};

const fetchPeriod = (
  id?: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(periodStartFetchAction());
    const result = await Api.agoraApi.getPeriod(id);
    const periodId = result.period.id;

    if (result.type === "proposal") {
      await dispatch(fetchProposals(periodId));
      await dispatch(fetchProposalVotes(periodId));
    }
    if (result.type === "promotion" || result.type === "exploration") {
      await dispatch(fetchBallots(periodId));
    }
    dispatch(periodSuccessFetchAction(result));
  };
};

const actionCreators = {
  fetchPeriod,
  fetchWelcomePage,
  fetchProposals,
  fetchMoreProposals,
  fetchProposalVotes,
  fetchMoreProposalVotes,
  fetchBallots,
  fetchMoreBallots,
};

const PeriodStore = {
  actions,
  actionCreators,
};

export default PeriodStore;
