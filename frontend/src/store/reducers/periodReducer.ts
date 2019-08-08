import { MetaPeriodInfo } from "~/models/Period";
import PeriodStore, {
  PeriodActionTypes,
  PeriodErrorFetchAction,
  PeriodSuccessFetchAction,
  ProposalBallotsSuccessFetchAction,
  ProposalsSuccessFetchAction,
  ProposalVotesSuccessFetchAction,
  ProposalNonVotersSuccessFetchAction,
  SpecificProposalVotesSuccessFetchAction,
} from "~/store/actions/periodActions";
import { ProposalBallotsListItem } from "~/models/ProposalBallotsList";
import { ProposalsList } from "~/models/ProposalsList";
import { ProposalVotesListItem } from "~/models/ProposalVotesList";
import { Decision } from "~/models/Decision";
import { Proposer } from "~/models/ProposalInfo";

interface Pagination {
  total: number;
  limit: number;
  rest: number;
  lastId: number;
}

export interface PeriodState {
  loading: boolean;
  proposalsLoading: boolean;
  proposalVotesLoading: boolean;
  ballotsLoading: boolean;
  period: MetaPeriodInfo | null;
  error: {
    errorCode: number;
    errorMessage: string;
  } | null;
  proposals: ProposalsList | null;
  proposalVotes: {
    pagination: Pagination;
    data: ProposalVotesListItem[];
    error: {
      errorCode: number;
      errorMessage: string;
    } | null;
  } | null;
  specificProposalVotes: {
    pagination: Pagination;
    data: ProposalVotesListItem[];
    error: {
      errorCode: number;
      errorMessage: string;
    } | null;
  } | null;
  ballotsDecisions: Decision[];
  ballots: {
    pagination: Pagination;
    data: ProposalBallotsListItem[];
    error: {
      errorCode: number;
      errorMessage: string;
    } | null;
  } | null;
  nonVoters: Proposer[] | null;
}

const initialState: PeriodState = {
  loading: true,
  proposalsLoading: false,
  proposalVotesLoading: false,
  ballotsLoading: false,
  period: null,
  error: null,
  proposals: null,
  proposalVotes: null,
  specificProposalVotes: null,
  ballots: null,
  ballotsDecisions: [],
  nonVoters: null,
};

export const periodReducer = (
  state: PeriodState = initialState,
  action: PeriodActionTypes
): PeriodState => {
  switch (action.type) {
    case PeriodStore.actions.PERIOD_START_FETCH:
      return {
        ...state,
        loading: true,
      };
    case PeriodStore.actions.PERIOD_SUCCESS_FETCH:
      return {
        ...state,
        loading: false,
        period: (action as PeriodSuccessFetchAction).payload.result,
        error: null,
      };
    case PeriodStore.actions.PERIOD_ERROR_FETCH:
      const error = (action as PeriodErrorFetchAction).payload;
      return {
        ...state,
        loading: false,
        period: null,
        error,
      };
    case PeriodStore.actions.PROPOSALS_START_FETCH:
      return {
        ...state,
        proposalsLoading: true,
      };
    case PeriodStore.actions.PROPOSALS_SUCCESS_FETCH:
      const proposalsAction = action as ProposalsSuccessFetchAction;
      return {
        ...state,
        proposalsLoading: false,
        proposals: proposalsAction.payload,
      };
    case PeriodStore.actions.PROPOSAL_VOTES_START_FETCH:
      return {
        ...state,
        proposalVotesLoading: true,
      };
    case PeriodStore.actions.PROPOSAL_VOTES_SUCCESS_FETCH:
      const proposalVotesAction = action as ProposalVotesSuccessFetchAction;
      if (proposalVotesAction.isLoadMore && state.proposalVotes) {
        return {
          ...state,
          proposalsLoading: false,
          proposalVotes: {
            pagination: proposalVotesAction.payload.pagination,
            data: [
              ...state.proposalVotes.data,
              ...proposalVotesAction.payload.results,
            ],
            error: null,
          },
        };
      }
      return {
        ...state,
        proposalsLoading: false,
        proposalVotes: {
          pagination: proposalVotesAction.payload.pagination,
          data: proposalVotesAction.payload.results,
          error: null,
        },
      };
    case PeriodStore.actions.SPECIFIC_PROPOSAL_VOTES_SUCCESS_FETCH:
      const specificProposalVotesAction = action as SpecificProposalVotesSuccessFetchAction;
      if (
        specificProposalVotesAction.isLoadMore &&
        state.specificProposalVotes
      ) {
        return {
          ...state,
          specificProposalVotes: {
            pagination: specificProposalVotesAction.payload.pagination,
            data: [
              ...state.specificProposalVotes.data,
              ...specificProposalVotesAction.payload.results,
            ],
            error: null,
          },
        };
      }
      return {
        ...state,
        specificProposalVotes: {
          pagination: specificProposalVotesAction.payload.pagination,
          data: specificProposalVotesAction.payload.results,
          error: null,
        },
      };
    case PeriodStore.actions.PROPOSAL_NONVOTERS_SUCCESS_FETCH:
      const nonVotersAction = action as ProposalNonVotersSuccessFetchAction;
      return { ...state, nonVoters: nonVotersAction.payload };
    case PeriodStore.actions.PROPOSAL_BALLOTS_START_FETCH:
      return {
        ...state,
        ballotsLoading: true,
      };
    case PeriodStore.actions.PROPOSAL_BALLOTS_SUCCESS_FETCH:
      const ballotsAction = action as ProposalBallotsSuccessFetchAction;
      if (ballotsAction.isLoadMore && state.ballots) {
        return {
          ...state,
          ballotsLoading: false,
          ballotsDecisions: ballotsAction.decisions,
          ballots: {
            pagination: ballotsAction.payload.pagination,
            data: [...state.ballots.data, ...ballotsAction.payload.results],
            error: null,
          },
        };
      }
      return {
        ...state,
        ballotsLoading: false,
        ballotsDecisions: ballotsAction.decisions,
        ballots: {
          pagination: ballotsAction.payload.pagination,
          data: ballotsAction.payload.results,
          error: null,
        },
      };
  }
  return state;
};
