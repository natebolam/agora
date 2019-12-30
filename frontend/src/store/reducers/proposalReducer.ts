import {
  ProposalActionTypes,
  ProposalErrorFetchAction,
  ProposalSuccessFetchAction,
} from "~/store/actions/proposalActions";
import { Proposal } from "~/models/ProposalInfo";
import { StageType, StageTimeInfo, VoteStats } from "~/models/Stage";

export interface ProposalState {
  isLoading: boolean;
  proposal?: Proposal;
  stage?: number;
  totalStages: number;
  stageType?: StageType;
  stageTimes?: StageTimeInfo;
  voteStats?: VoteStats;
  winner?: Proposal;
  error?: {
    errorCode: number;
    errorMessage: string;
  };
}

const initialState: ProposalState = {
  isLoading: false,
  proposal: undefined,
  stage: undefined,
  totalStages: 0,
  stageType: undefined,
  stageTimes: undefined,
  error: undefined,
};

const proposalReducer = (
  state: ProposalState = initialState,
  action: ProposalActionTypes
): ProposalState => {
  switch (action.type) {
    case "@@proposal/start_fetch":
      return {
        isLoading: true,
        proposal: undefined,
        totalStages: 0,
      };
    case "@@proposal/success_fetch":
      return {
        isLoading: false,
        proposal: (action as ProposalSuccessFetchAction).payload.proposal,
        stage: (action as ProposalSuccessFetchAction).payload.stage,
        totalStages: (action as ProposalSuccessFetchAction).payload.totalStages,
        stageType: (action as ProposalSuccessFetchAction).payload.stageType,
        stageTimes: (action as ProposalSuccessFetchAction).payload.stageTimes,
        voteStats: (action as ProposalSuccessFetchAction).payload.voteStats,
        winner: (action as ProposalSuccessFetchAction).payload.winner,
      };
    case "@@proposal/error_fetch":
      return {
        isLoading: false,
        proposal: undefined,
        totalStages: 0,
        error: {
          errorCode: (action as ProposalErrorFetchAction).payload.errorCode,
          errorMessage: (action as ProposalErrorFetchAction).payload
            .errorMessage,
        },
      };
    default:
      return state;
  }
};

export default proposalReducer;
