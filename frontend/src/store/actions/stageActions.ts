import { MetaStageInfo } from "~/models/Stage";
import { ThunkAction } from "redux-thunk";
import { RootStoreType } from "~/store";
import { Action } from "redux";
import { Api } from "~/api/api";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import { ProposalsList } from "~/models/ProposalInfo";

const PERIOD_START_FETCH = "@@stage/start_fetch";
const PERIOD_SUCCESS_FETCH = "@@stage/success_fetch";
const PERIOD_ERROR_FETCH = "@@stage/error_fetch";

const PROPOSALS_START_FETCH = "@@stage/proposals/start_fetch";
const PROPOSALS_SUCCESS_FETCH = "@@stage/proposals/success_fetch";
const PROPOSALS_ERROR_FETCH = "@@stage/proposals/error_fetch";

const PROPOSAL_VOTES_START_FETCH = "@@stage/proposal_votes/start_fetch";
const PROPOSAL_VOTES_SUCCESS_FETCH = "@@stage/proposal_votes/success_fetch";
const PROPOSAL_VOTES_ERROR_FETCH = "@@stage/proposal_votes/error_fetch";

const SPECIFIC_PROPOSAL_VOTES_START_FETCH =
  "@@stage/specific_proposal_votes/start_fetch";
const SPECIFIC_PROPOSAL_VOTES_SUCCESS_FETCH =
  "@@stage/specific_proposal_votes/success_fetch";
const SPECIFIC_PROPOSAL_VOTES_ERROR_FETCH =
  "@@stage/specific_proposal_votes/error_fetch";

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
};

export interface StageStartFetchAction {
  type: typeof PERIOD_START_FETCH;
}

export interface StageSuccessFetchAction {
  type: typeof PERIOD_SUCCESS_FETCH;
  payload: {
    result: MetaStageInfo;
  };
}

export interface StageErrorFetchAction {
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

export type StageActionTypes =
  | StageStartFetchAction
  | StageSuccessFetchAction
  | StageErrorFetchAction
  | ProposalsStartFetchAction
  | ProposalsSuccessFetchAction
  | ProposalsErrorFetchAction
  | ProposalVotesStartFetchAction
  | ProposalVotesSuccessFetchAction
  | ProposalVotesErrorFetchAction
  | SpecificProposalVotesStartFetchAction
  | SpecificProposalVotesSuccessFetchAction
  | SpecificProposalVotesErrorFetchAction;

const stageStartFetchAction = (): StageStartFetchAction => {
  return {
    type: PERIOD_START_FETCH,
  };
};

const stageSuccessFetchAction = (
  result: MetaStageInfo
): StageSuccessFetchAction => {
  return {
    type: PERIOD_SUCCESS_FETCH,
    payload: {
      result,
    },
  };
};

const stageErrorFetchAction = (
  errorCode: number,
  errorMessage: string
): StageErrorFetchAction => {
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

export const fetchProposals = (
  stageId: number,
  lastId?: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalsStartFetchAction());
    const result = await Api.agoraApi.getProposals(stageId, lastId);
    dispatch(await proposalsSuccessFetchAction(result, lastId !== undefined));
  };
};

export const fetchProposalVotes = (
  stageId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalVotesStartFetchAction());
    const result = await Api.agoraApi.getProposalVotes(stageId);
    dispatch(proposalVotesSuccessFetchAction(result));
  };
};

export const fetchSpecificProposalVotes = (
  stageId: number,
  proposalId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    const result = await Api.agoraApi.getSpecificProposalVotes(
      stageId,
      proposalId
    );
    dispatch(specificProposalVotesSuccessFetchAction(result));
  };
};

const fetchStage = (
  id?: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(stageStartFetchAction());
    try {
      const result = await Api.agoraApi.getStage(id);
      const stageId = result.stage;
      if (result.type === "implementation" || result.type === "proposal") {
        await dispatch(await fetchProposals(stageId));
      }
      if (result.type === "voting") {
        await dispatch(await fetchProposals(stageId));
        await dispatch(await fetchProposalVotes(stageId));
      }
      await dispatch(await stageSuccessFetchAction(result));
    } catch (e) {
      if (e.response) {
        dispatch(
          stageErrorFetchAction(e.response.status, e.response.statusText)
        );
      } else {
        dispatch(stageErrorFetchAction(404, ""));
      }
    }
  };
};

const actionCreators = {
  fetchStage,
  fetchProposals,
};

const StageStore = {
  actions,
  actionCreators,
};

export default StageStore;
