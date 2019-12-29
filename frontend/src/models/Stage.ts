import { Proposal } from "~/models/ProposalInfo";

export type MetaStageInfo =
  | ProposalStageInfo
  | EvaluationStageInfo
  | VotingStageInfo
  | ImplementationStageInfo;

export interface BallotsStats {
  yay: number;
  nay: number;
  pass: number;
  quorum: number;
  supermajority: number;
}

export interface VoteStats {
  numVoters: number;
  numVotersTotal: number;
}

export interface StageTime {
  stage: number;
  stageType: "proposal" | "testing_vote" | "voting_for_vote" | "implementation";
}
export type StageTimeInfo = StageTime[];

export type StageType = "proposal" | "evaluation" | "voting" | "implementation";

interface StageInfo {
  type: StageType;
  stage: number;
  totalStages: number;
  stageTimes: StageTimeInfo;
  discourseLink: string;
}

export interface StageWithProposalInfo extends StageInfo {
  proposal: Proposal;
}

export interface VotingStageInfo extends StageInfo {
  voteStats: VoteStats;
  winner: Proposal;
}

export type ImplementationStageInfo = StageWithProposalInfo;

export type ProposalStageInfo = StageInfo;

export type EvaluationStageInfo = StageInfo;
