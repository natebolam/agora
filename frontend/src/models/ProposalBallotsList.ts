import { Pagination } from "~/models/Pagination";
import { Decision } from "~/models/Decision";
import { Proposer } from "~/models/ProposalInfo";

export interface ProposalBallotsListItem {
  id: number;
  author: Proposer;
  decision: Decision;
  operation: string;
  timestamp: string;
}

export interface ProposalBallotsList {
  pagination: Pagination;
  results: ProposalBallotsListItem[];
}
