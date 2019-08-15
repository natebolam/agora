import { Proposal } from "~/models/ProposalInfo";

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

export interface VoteStats {
  votesCast: number;
  votesAvailable: number;
  numVoters: number;
}

export type PeriodType = "proposal" | "exploration" | "testing" | "promotion";

interface PeriodInfo {
  type: PeriodType;
  period: Period;
  discourseLink: string;
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
