import { Proposer } from "~/models/Period";
import { Pagination } from "~/models/Pagination";

export interface ProposalsListItem {
  id: number;
  discourseLink: string;
  hash: string;
  title: string;
  shortDescription: string;
  longDescription: string;
  proposer: Proposer;
  timeCreated: string;
  votesCasted: number;
}

export interface ProposalsList {
  pagination: Pagination;
  results: ProposalsListItem[];
}
