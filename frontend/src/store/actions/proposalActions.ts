import { ThunkAction } from "redux-thunk";
import { RootStoreType } from "~/store";
import { Action } from "redux";
import { Api } from "~/api/api";
import { Proposal } from "~/models/ProposalInfo";
import { Period, PeriodType } from "~/models/Period";

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
  periodType: PeriodType
): ProposalSuccessFetchAction => {
  return {
    type: PROPOSAL_SUCCESS_FETCH,
    payload: {
      proposal,
      period,
      totalPeriods,
      periodType,
    },
  };
};

const fetchProposal = (
  proposalId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalStartFetchAction());
    const proposal = await Api.agoraApi.getProposal(proposalId);
    const period = await Api.agoraApi.getPeriod(proposal.period);
    dispatch(
      proposalSuccessFetchAction(
        proposal,
        period.period,
        period.totalPeriods,
        period.type
      )
    );
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
