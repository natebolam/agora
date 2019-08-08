import { MetaPeriodInfo } from "~/models/Period";
import { ThunkAction } from "redux-thunk";
import { RootStoreType } from "~/store";
import { Action } from "redux";
import { Api } from "~/api/api";
import { ProposalsList } from "~/models/ProposalsList";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { Decision } from "~/models/Decision";
import { Proposer } from "~/models/ProposalInfo";

const PERIOD_START_FETCH = "@@period/start_fetch";
const PERIOD_SUCCESS_FETCH = "@@period/success_fetch";
const PERIOD_ERROR_FETCH = "@@period/error_fetch";

const PROPOSALS_START_FETCH = "@@period/proposals/start_fetch";
const PROPOSALS_SUCCESS_FETCH = "@@period/proposals/success_fetch";
const PROPOSALS_ERROR_FETCH = "@@period/proposals/error_fetch";

const PROPOSAL_VOTES_START_FETCH = "@@period/proposal_votes/start_fetch";
const PROPOSAL_VOTES_SUCCESS_FETCH = "@@period/proposal_votes/success_fetch";
const PROPOSAL_VOTES_ERROR_FETCH = "@@period/proposal_votes/error_fetch";

const SPECIFIC_PROPOSAL_VOTES_START_FETCH =
  "@@period/specific_proposal_votes/start_fetch";
const SPECIFIC_PROPOSAL_VOTES_SUCCESS_FETCH =
  "@@period/specific_proposal_votes/success_fetch";
const SPECIFIC_PROPOSAL_VOTES_ERROR_FETCH =
  "@@period/specific_proposal_votes/error_fetch";

const PROPOSAL_BALLOTS_START_FETCH = "@@period/proposal_ballots/start_fetch";
const PROPOSAL_BALLOTS_SUCCESS_FETCH =
  "@@period/proposal_ballots/success_fetch";
const PROPOSAL_BALLOTS_ERROR_FETCH = "@@period/proposal_ballots/error_fetch";

const PROPOSAL_NONVOTERS_SUCCESS_FETCH =
  "@@period/proposal_nonvoters/success_fetch";

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
  SPECIFIC_PROPOSAL_VOTES_START_FETCH,
  SPECIFIC_PROPOSAL_VOTES_SUCCESS_FETCH,
  SPECIFIC_PROPOSAL_VOTES_ERROR_FETCH,
  PROPOSAL_NONVOTERS_SUCCESS_FETCH,
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

export interface SpecificProposalVotesStartFetchAction {
  type: typeof SPECIFIC_PROPOSAL_VOTES_START_FETCH;
}

export interface SpecificProposalVotesSuccessFetchAction {
  type: typeof SPECIFIC_PROPOSAL_VOTES_SUCCESS_FETCH;
  payload: ProposalVotesList;
  isLoadMore: boolean;
}

export interface SpecificProposalVotesErrorFetchAction {
  type: typeof SPECIFIC_PROPOSAL_VOTES_ERROR_FETCH;
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
  decisions: Decision[];
}

export interface ProposalBallotsErrorFetchAction {
  type: typeof PROPOSAL_BALLOTS_ERROR_FETCH;
  payload: {
    errorCode: number;
    errorMessage: string;
  };
}

export interface ProposalNonVotersSuccessFetchAction {
  type: typeof PROPOSAL_NONVOTERS_SUCCESS_FETCH;
  payload: Proposer[];
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
  | SpecificProposalVotesStartFetchAction
  | SpecificProposalVotesSuccessFetchAction
  | SpecificProposalVotesErrorFetchAction
  | ProposalNonVotersSuccessFetchAction
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

const periodErrorFetchAction = (
  errorCode: number,
  errorMessage: string
): PeriodErrorFetchAction => {
  return {
    type: PERIOD_ERROR_FETCH,
    payload: {
      errorCode,
      errorMessage,
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

const specificProposalVotesSuccessFetchAction = (
  result: ProposalVotesList,
  isLoadMore: boolean = false
): SpecificProposalVotesSuccessFetchAction => {
  return {
    type: SPECIFIC_PROPOSAL_VOTES_SUCCESS_FETCH,
    payload: result,
    isLoadMore,
  };
};

const proposalBallotsSuccessFetchAction = (
  result: ProposalBallotsList,
  decisions: Decision[],
  isLoadMore: boolean = false
): ProposalBallotsSuccessFetchAction => {
  return {
    type: PROPOSAL_BALLOTS_SUCCESS_FETCH,
    payload: result,
    isLoadMore,
    decisions,
  };
};

const proposalNonVotersSuccessFetchAction = (
  result: Proposer[]
): ProposalNonVotersSuccessFetchAction => {
  return {
    type: PROPOSAL_NONVOTERS_SUCCESS_FETCH,
    payload: result,
  };
};

export const fetchProposals = (
  periodId: number,
  lastId?: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalsStartFetchAction());
    const result = await Api.agoraApi.getProposals(periodId, lastId);
    dispatch(await proposalsSuccessFetchAction(result, lastId !== undefined));
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

export const fetchSpecificProposalVotes = (
  proposalId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    const result = await Api.agoraApi.getSpecificProposalVotes(proposalId);
    dispatch(specificProposalVotesSuccessFetchAction(result));
  };
};

export const fetchRestSpecificProposalVotes = async (
  state: RootStoreType
): Promise<void | SpecificProposalVotesSuccessFetchAction> => {
  const proposalInfo = state.proposalStore.proposal;
  const specificProposalVotes = state.periodStore.specificProposalVotes;

  if (proposalInfo && specificProposalVotes) {
    const result = await Api.agoraApi.getSpecificProposalVotes(
      proposalInfo.id,
      specificProposalVotes.pagination.lastId,
      specificProposalVotes.pagination.rest
    );
    return specificProposalVotesSuccessFetchAction(result, true);
  }
};

export const fetchBallots = (
  periodId: number,
  decisions: Decision[] = []
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    const result = await Api.agoraApi.getBallots(periodId, decisions);
    dispatch(proposalBallotsSuccessFetchAction(result, decisions));
  };
};

export const fetchRestBallots = async (
  state: RootStoreType
): Promise<void | ProposalBallotsSuccessFetchAction> => {
  const periodInfo = state.periodStore.period;
  const ballots = state.periodStore.ballots;
  const ballotsDecisions = state.periodStore.ballotsDecisions;

  if (periodInfo && periodInfo.period && ballots && ballots.pagination.rest) {
    const result = await Api.agoraApi.getBallots(
      periodInfo.period.id,
      ballotsDecisions,
      ballots.pagination.lastId,
      ballots.pagination.rest
    );
    return proposalBallotsSuccessFetchAction(result, ballotsDecisions, true);
  }
};

export const fetchNonVoters = (
  periodId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    const result = await Api.agoraApi.getNonVoters(periodId);
    dispatch(proposalNonVotersSuccessFetchAction(result));
  };
};

const fetchWelcomePage = (): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(periodStartFetchAction());
    try {
      const result = await Api.agoraApi.getPeriod();
      await dispatch(periodSuccessFetchAction(result));
    } catch (e) {
      if (e.response) {
        dispatch(
          periodErrorFetchAction(e.response.status, e.response.statusText)
        );
      } else {
        dispatch(periodErrorFetchAction(404, ""));
      }
    }
  };
};

const fetchPeriod = (
  id?: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(periodStartFetchAction());
    try {
      const result = await Api.agoraApi.getPeriod(id);
      const periodId = result.period.id;
      if (result.type === "proposal") {
        await dispatch(await fetchProposals(periodId));
        await dispatch(await fetchProposalVotes(periodId));
      }
      if (result.type === "promotion" || result.type === "exploration") {
        await dispatch(await fetchBallots(periodId));
        await dispatch(await fetchNonVoters(periodId));
      }
      await dispatch(await periodSuccessFetchAction(result));
    } catch (e) {
      if (e.response) {
        dispatch(
          periodErrorFetchAction(e.response.status, e.response.statusText)
        );
      } else {
        dispatch(periodErrorFetchAction(404, ""));
      }
    }
  };
};

const actionCreators = {
  fetchPeriod,
  fetchWelcomePage,
  fetchProposals,
};

const PeriodStore = {
  actions,
  actionCreators,
};

export default PeriodStore;
