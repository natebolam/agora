import {
  ProposalActionTypes,
  ProposalErrorFetchAction,
  ProposalSuccessFetchAction,
} from "~/store/actions/proposalActions";
import { Proposal } from "~/models/ProposalInfo";
import { Period, PeriodType } from "~/models/Period";

export interface ProposalState {
  isLoading: boolean;
  proposal?: Proposal;
  period?: Period;
  totalPeriods: number;
  periodType?: PeriodType;
  error?: {
    errorCode: number;
    errorMessage: string;
  };
}

const initialState: ProposalState = {
  isLoading: false,
  proposal: undefined,
  period: undefined,
  totalPeriods: 0,
  periodType: undefined,
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
        totalPeriods: 0,
      };
    case "@@proposal/success_fetch":
      return {
        isLoading: false,
        proposal: (action as ProposalSuccessFetchAction).payload.proposal,
        period: (action as ProposalSuccessFetchAction).payload.period,
        totalPeriods: (action as ProposalSuccessFetchAction).payload
          .totalPeriods,
        periodType: (action as ProposalSuccessFetchAction).payload.periodType,
      };
    case "@@proposal/error_fetch":
      return {
        isLoading: false,
        proposal: undefined,
        totalPeriods: 0,
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
