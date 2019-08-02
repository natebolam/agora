export interface Proposer {
  pkh: string;
  rolls: number;
  name: string;
  logoUrl: string | null;
}

export interface Proposal {
  id: number;
  hash: string;
  title: string;
  shortDescription: string;
  longDescription: string;
  timeCreated: string;
  proposalFile: string | null;
  discourseLink: string | null;
  proposer: Proposer;
  period: number;
}
