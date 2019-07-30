import { Proposal } from "~/models/Period";
import { ThunkAction } from "redux-thunk";
import { RootStoreType } from "~/store";
import { Action } from "redux";
import { Api } from "~/api/api";

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
  payload: Proposal;
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
  result: Proposal
): ProposalSuccessFetchAction => {
  return {
    type: PROPOSAL_SUCCESS_FETCH,
    payload: result,
  };
};

const fetchProposal = (
  proposalId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalStartFetchAction());
    const result = await Api.agoraApi.getProposal(proposalId);
    dispatch(proposalSuccessFetchAction(result));
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
