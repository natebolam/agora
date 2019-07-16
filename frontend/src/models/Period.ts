export type MetaPeriodInfo =
  | ProposalPeriodInfo
  | ExplorationPeriodInfo
  | TestingPeriodInfo
  | PromotionPeriodInfo;

export interface Period {
  id: number;
  startLevel: number;
  endLevel: number;
  startTime: string;
  endTime: string;
  cycle: number;
}

export interface BallotsStats {
  yay: number;
  nay: number;
  pass: number;
  quorum: number;
  supermajority: number;
}

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
}

export interface VoteStats {
  votesCast: number;
  votesAvailable: number;
}

export type ProposalType = "proposal" | "exploration" | "testing" | "promotion";

interface PeriodInfo {
  type: ProposalType;
  period: Period;
  totalPeriods: number;
}

export interface ProposalPeriodInfo extends PeriodInfo {
  voteStats: VoteStats;
}

export interface TestingPeriodInfo extends PeriodInfo {
  proposal: Proposal;
}

export interface ExplorationPeriodInfo extends PeriodInfo {
  proposal: Proposal;
  ballots: BallotsStats;
  voteStats: VoteStats;
}

export interface PromotionPeriodInfo extends PeriodInfo {
  proposal: Proposal;
  ballots: BallotsStats;
  voteStats: VoteStats;
}
