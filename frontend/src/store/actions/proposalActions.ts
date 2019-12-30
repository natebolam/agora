import { ThunkAction } from "redux-thunk";
import { RootStoreType } from "~/store";
import { Action } from "redux";
import { Api } from "~/api/api";
import { Proposal } from "~/models/ProposalInfo";
import {
  StageType,
  StageTimeInfo,
  VoteStats,
  VotingStageInfo,
} from "~/models/Stage";
import { fetchSpecificProposalVotes } from "./stageActions";

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
    stage: number;
    totalStages: number;
    stageType: StageType;
    stageTimes: StageTimeInfo;
    voteStats: VoteStats;
    winner: Proposal;
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
  stage: number,
  totalStages: number,
  stageType: StageType,
  stageTimes: StageTimeInfo,
  voteStats: VoteStats,
  winner: Proposal
): ProposalSuccessFetchAction => {
  return {
    type: PROPOSAL_SUCCESS_FETCH,
    payload: {
      proposal,
      stage,
      totalStages,
      stageType,
      stageTimes,
      voteStats,
      winner,
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
  stageId: number,
  proposalId: number
): ThunkAction<void, RootStoreType, null, Action> => {
  return async (dispatch): Promise<void> => {
    dispatch(proposalStartFetchAction());
    try {
      const proposal = await Api.agoraApi.getProposal(proposalId, stageId);
      const stage = await Api.agoraApi.getStage(proposal.stage);
      await dispatch(await fetchSpecificProposalVotes(stageId, proposalId));
      dispatch(
        proposalSuccessFetchAction(
          proposal,
          stage.stage,
          stage.totalStages,
          stage.type,
          stage.stageTimes,
          (stage as VotingStageInfo).voteStats,
          (stage as VotingStageInfo).winner
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
