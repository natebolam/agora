export interface Proposer {
  pkh: string;
  rolls: number;
  name: string;
  logoUrl: string | null;
  profileUrl: string | null;
}

export interface Proposal {
  id: number;
  hash: string;
  title: string;
  shortDescription: string;
  longDescription: string;
  timeCreated: string;
  proposalFile: string | null;
  discourseLink: string;
  proposer: Proposer;
  period: number;
}
