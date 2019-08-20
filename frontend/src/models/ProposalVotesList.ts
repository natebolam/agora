import { Pagination } from "~/models/Pagination";
import { Proposer } from "~/models/ProposalInfo";

export interface ProposalVotesListItem {
  id: number;
  author: Proposer;
  operation: string;
  proposal: string;
  proposalTitle: string | null;
  timestamp: string;
}

export interface ProposalVotesList {
  pagination: Pagination;
  results: ProposalVotesListItem[];
}
