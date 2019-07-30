import { Proposal } from "~/models/Period";
import {
  ProposalActionTypes,
  ProposalErrorFetchAction,
  ProposalSuccessFetchAction,
} from "~/store/actions/proposalActions";

export interface ProposalState {
  isLoading: boolean;
  proposal?: Proposal;
  error?: {
    errorCode: number;
    errorMessage: string;
  };
}

const initialState: ProposalState = {
  isLoading: false,
  proposal: undefined,
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
      };
    case "@@proposal/success_fetch":
      return {
        isLoading: false,
        proposal: (action as ProposalSuccessFetchAction).payload,
      };
    case "@@proposal/error_fetch":
      return {
        isLoading: false,
        proposal: undefined,
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
