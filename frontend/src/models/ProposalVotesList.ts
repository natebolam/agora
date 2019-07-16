import { Proposer } from "~/models/Period";
import { Pagination } from "~/models/Pagination";

export interface ProposalVotesListItem {
  id: number;
  author: Proposer;
  operation: string;
  timestamp: string;
}

export interface ProposalVotesList {
  pagination: Pagination;
  results: ProposalVotesListItem[];
}
