import { Proposer } from "~/models/Period";
import { Pagination } from "~/models/Pagination";

export interface ProposalsListItem {
  id: number;
  author: Proposer;
  operation: string;
  timestamp: string;
}

export interface ProposalsList {
  pagination: Pagination;
  results: ProposalsListItem[];
}
