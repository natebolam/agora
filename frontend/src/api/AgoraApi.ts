import { AxiosInstance, AxiosResponse } from "axios";
import {
  MetaStageInfo,
  VotingStageInfo,
  ImplementationStageInfo,
  ProposalStageInfo,
  EvaluationStageInfo,
} from "~/models/Stage";
import { Proposal, ProposalsList } from "~/models/ProposalInfo";
import { ProposalVotesList } from "~/models/ProposalVotesList";

interface StageResponse {
  proposalInfo?: ProposalStageInfo;
  evaluationInfo?: EvaluationStageInfo;
  votingInfo?: VotingStageInfo;
  implementationInfo?: ImplementationStageInfo;
}

interface AgoraApiType {
  getStage: (id?: number) => Promise<MetaStageInfo>;
  getProposals: (
    stageId: number,
    lastId?: number,
    limit?: number
  ) => Promise<ProposalsList>;
  getProposalVotes: (stageId: number) => Promise<ProposalVotesList>;
  getSpecificProposalVotes: (
    stageId: number,
    proposalId: number
  ) => Promise<ProposalVotesList>;
  getProposal: (proposalId: number, stageId: number) => Promise<Proposal>;
}

const convertStage = (stageResponse: StageResponse): MetaStageInfo => {
  if (stageResponse.proposalInfo) {
    return {
      type: "proposal",
      ...stageResponse.proposalInfo,
    };
  }
  if (stageResponse.evaluationInfo) {
    return {
      type: "evaluation",
      ...stageResponse.evaluationInfo,
    };
  }
  if (stageResponse.votingInfo) {
    return {
      type: "voting",
      ...stageResponse.votingInfo,
    };
  }
  if (stageResponse.implementationInfo) {
    return {
      type: "implementation",
      ...stageResponse.implementationInfo,
    };
  }
  throw new Error();
};

export function AgoraApi(axios: AxiosInstance): AgoraApiType {
  return {
    getStage: async (id?: number): Promise<MetaStageInfo> => {
      return axios
        .get("/stage", {
          params: {
            id,
          },
        })
        .then(
          (response: AxiosResponse<StageResponse>): MetaStageInfo => {
            return convertStage(response.data);
          }
        );
    },
    getProposals: async (stageId: number): Promise<ProposalsList> => {
      return axios.get(`/proposals/${stageId}`).then(
        (response: AxiosResponse<ProposalsList>): ProposalsList => {
          return response.data;
        }
      );
    },
    getSpecificProposalVotes: async (
      stageId: number,
      proposalId: number
    ): Promise<ProposalVotesList> => {
      return axios
        .get(`/proposal_votes/stage/${stageId}/proposal/${proposalId}/votes`)
        .then(
          (response: AxiosResponse<ProposalVotesList>): ProposalVotesList => {
            return response.data;
          }
        );
    },
    getProposalVotes: async (stageId: number): Promise<ProposalVotesList> => {
      return axios.get(`/votes/stage/${stageId}`).then(
        (response: AxiosResponse<ProposalVotesList>): ProposalVotesList => {
          return response.data;
        }
      );
    },
    getProposal: async (
      proposalId: number,
      stageId: number
    ): Promise<Proposal> => {
      return axios
        .get(`/proposal/${proposalId}/stage/${stageId}`)
        .then((response: AxiosResponse<Proposal>): Proposal => response.data);
    },
  };
}
