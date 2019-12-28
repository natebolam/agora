import { MetaStageInfo } from "~/models/Stage";
import StageStore, {
  StageActionTypes,
  StageErrorFetchAction,
  StageSuccessFetchAction,
  ProposalsSuccessFetchAction,
  ProposalVotesSuccessFetchAction,
  SpecificProposalVotesSuccessFetchAction,
} from "~/store/actions/stageActions";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import { ProposalsList } from "~/models/ProposalInfo";

export interface StageState {
  loading: boolean;
  proposalsLoading: boolean;
  proposalVotesLoading: boolean;
  stage: MetaStageInfo | null;
  error: {
    errorCode: number;
    errorMessage: string;
  } | null;
  proposals: ProposalsList | null;
  proposalVotes: ProposalVotesList | null;
  specificProposalVotes: ProposalVotesList | null;
}

const initialState: StageState = {
  loading: true,
  proposalsLoading: false,
  proposalVotesLoading: false,
  stage: null,
  error: null,
  proposals: null,
  proposalVotes: null,
  specificProposalVotes: null,
};

export const stageReducer = (
  state: StageState = initialState,
  action: StageActionTypes
): StageState => {
  switch (action.type) {
    case StageStore.actions.PERIOD_START_FETCH:
      return {
        ...state,
        loading: true,
      };
    case StageStore.actions.PERIOD_SUCCESS_FETCH:
      return {
        ...state,
        loading: false,
        stage: (action as StageSuccessFetchAction).payload.result,
        error: null,
      };
    case StageStore.actions.PERIOD_ERROR_FETCH:
      const error = (action as StageErrorFetchAction).payload;
      return {
        ...state,
        loading: false,
        stage: null,
        error,
      };
    case StageStore.actions.PROPOSALS_START_FETCH:
      return {
        ...state,
        proposalsLoading: true,
      };
    case StageStore.actions.PROPOSALS_SUCCESS_FETCH:
      const proposalsAction = action as ProposalsSuccessFetchAction;
      return {
        ...state,
        proposalsLoading: false,
        proposals: proposalsAction.payload,
      };
    case StageStore.actions.PROPOSAL_VOTES_START_FETCH:
      return {
        ...state,
        proposalVotesLoading: true,
      };
    case StageStore.actions.PROPOSAL_VOTES_SUCCESS_FETCH:
      const proposalVotesAction = action as ProposalVotesSuccessFetchAction;
      if (proposalVotesAction.isLoadMore && state.proposalVotes) {
        return {
          ...state,
          proposalsLoading: false,
          proposalVotes: proposalVotesAction.payload,
        };
      }
      return {
        ...state,
        proposalsLoading: false,
        proposalVotes: proposalVotesAction.payload,
      };
    case StageStore.actions.SPECIFIC_PROPOSAL_VOTES_SUCCESS_FETCH:
      const specificProposalVotesAction = action as SpecificProposalVotesSuccessFetchAction;
      if (
        specificProposalVotesAction.isLoadMore &&
        state.specificProposalVotes
      ) {
        return {
          ...state,
          specificProposalVotes: specificProposalVotesAction.payload,
        };
      }
      return {
        ...state,
        specificProposalVotes: specificProposalVotesAction.payload,
      };
  }
  return state;
};
