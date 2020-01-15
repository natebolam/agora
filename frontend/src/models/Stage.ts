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

export type StageTypeAction =
  | "proposal"
  | "testing_vote"
  | "voting_for_vote"
  | "implementation";

export interface StageTime {
  stage: number;
  stageType: StageTypeAction;
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

export interface StageWithPossibleWinner extends StageInfo {
  winner: Proposal;
}

export interface VotingStageInfo extends StageWithPossibleWinner {
  voteStats: VoteStats;
}

export type ImplementationStageInfo = StageWithProposalInfo;

export type ProposalStageInfo = StageWithPossibleWinner;

export type EvaluationStageInfo = StageWithPossibleWinner;
