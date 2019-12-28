export interface ProposalVotesListItem {
  id: number;
  proposal: string;
  proposalTitle: string | null;
  author: string;
  timestamp: string;
}

export type ProposalVotesList = ProposalVotesListItem[];
