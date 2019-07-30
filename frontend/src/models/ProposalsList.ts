import { Proposer } from "~/models/ProposalInfo";

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

export type ProposalsList = ProposalsListItem[];
