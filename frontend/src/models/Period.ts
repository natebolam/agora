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
  curLevel: number;
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
  numVotersTotal: number;
}

export interface PeriodTime {
  startTime: string;
  endTime: string;
  periodType: "proposal" | "testing_vote" | "testing" | "promotion_vote";
}
export type PeriodTimeInfo = PeriodTime[];

export type PeriodType = "proposal" | "exploration" | "testing" | "promotion";

interface PeriodInfo {
  type: PeriodType;
  period: Period;
  discourseLink: string;
  totalPeriods: number;
  periodTimes: PeriodTimeInfo;
  advanced: boolean;
}

export interface PeriodWithProposalInfo extends PeriodInfo {
  proposal: Proposal;
}

export interface ProposalPeriodInfo extends PeriodInfo {
  voteStats: VoteStats;
  winner: Proposal;
}

export type TestingPeriodInfo = PeriodWithProposalInfo;

export interface ExplorationPeriodInfo extends PeriodWithProposalInfo {
  ballots: BallotsStats;
  voteStats: VoteStats;
}

export interface PromotionPeriodInfo extends PeriodWithProposalInfo {
  ballots: BallotsStats;
  voteStats: VoteStats;
}
