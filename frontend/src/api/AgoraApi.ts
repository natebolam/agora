import { AxiosInstance, AxiosResponse } from "axios";
import {
  ExplorationPeriodInfo,
  MetaPeriodInfo,
  PromotionPeriodInfo,
  ProposalPeriodInfo,
  TestingPeriodInfo,
} from "~/models/Period";
import { ProposalsList } from "~/models/ProposalsList";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import { Decision } from "~/models/Decision";
import { Proposal, Proposer } from "~/models/ProposalInfo";

interface PeriodResponse {
  proposalInfo?: ProposalPeriodInfo;
  explorationInfo?: ExplorationPeriodInfo;
  testingInfo?: TestingPeriodInfo;
  promotionInfo?: PromotionPeriodInfo;
}

interface AgoraApiType {
  getPeriod: (id?: number) => Promise<MetaPeriodInfo>;
  getProposals: (
    periodId: number,
    lastId?: number,
    limit?: number
  ) => Promise<ProposalsList>;
  getProposalVotes: (
    periodId: number,
    lastId?: number,
    limit?: number
  ) => Promise<ProposalVotesList>;
  getSpecificProposalVotes: (
    proposalId: number,
    lastId?: number,
    limit?: number
  ) => Promise<ProposalVotesList>;
  getBallots: (
    periodId: number,
    decisions: Decision[],
    lastId?: number,
    limit?: number
  ) => Promise<ProposalBallotsList>;
  getNonVoters: (periodId: number) => Promise<Proposer[]>;
  getProposal: (proposalId: number) => Promise<Proposal>;
}

const convertPeriod = (periodResponse: PeriodResponse): MetaPeriodInfo => {
  if (periodResponse.proposalInfo) {
    return {
      type: "proposal",
      ...periodResponse.proposalInfo,
    };
  }
  if (periodResponse.testingInfo) {
    return {
      type: "testing",
      ...periodResponse.testingInfo,
    };
  }
  if (periodResponse.explorationInfo) {
    return {
      type: "exploration",
      ...periodResponse.explorationInfo,
    };
  }
  if (periodResponse.promotionInfo) {
    return {
      type: "promotion",
      ...periodResponse.promotionInfo,
    };
  }
  throw new Error();
};

export function AgoraApi(axios: AxiosInstance): AgoraApiType {
  return {
    getPeriod: async (id?: number): Promise<MetaPeriodInfo> => {
      return axios
        .get("/period", {
          params: {
            id,
          },
        })
        .then(
          (response: AxiosResponse<PeriodResponse>): MetaPeriodInfo => {
            return convertPeriod(response.data);
          }
        );
    },
    getProposals: async (
      periodId: number,
      lastId?: number,
      limit: number = 20
    ): Promise<ProposalsList> => {
      return axios
        .get(`/proposals/${periodId}`, {
          params: {
            limit,
            lastId,
          },
        })
        .then(
          (response: AxiosResponse<ProposalsList>): ProposalsList => {
            return response.data;
          }
        );
    },
    getProposalVotes: async (
      periodId: number,
      lastId?: number,
      limit: number = 4
    ): Promise<ProposalVotesList> => {
      return axios
        .get(`/proposal_votes/${periodId}`, {
          params: { limit, lastId },
        })
        .then(
          (response: AxiosResponse<ProposalVotesList>): ProposalVotesList => {
            return response.data;
          }
        );
    },
    getSpecificProposalVotes: async (
      proposalId: number,
      lastId?: number,
      limit: number = 10
    ): Promise<ProposalVotesList> => {
      return axios
        .get(`/proposal/${proposalId}/votes`, {
          params: { limit, lastId },
        })
        .then(
          (response: AxiosResponse<ProposalVotesList>): ProposalVotesList => {
            return response.data;
          }
        );
    },
    getBallots: async (
      periodId: number,
      decisions: Decision[],
      lastId?: number,
      limit: number = 10
    ): Promise<ProposalBallotsList> => {
      const serializedDecisions = decisions.length
        ? `[${decisions.toString()}]`
        : void 0;
      return axios
        .get(`/ballots/${periodId}`, {
          params: { limit, lastId, decisions: serializedDecisions },
        })
        .then(
          (
            response: AxiosResponse<ProposalBallotsList>
          ): ProposalBallotsList => {
            return response.data;
          }
        );
    },
    getNonVoters: async (periodId: number): Promise<Proposer[]> => {
      return axios
        .get(`/non_voters/${periodId}`)
        .then((response: AxiosResponse<Proposer[]>): Proposer[] => {
          return response.data;
        });
    },
    getProposal: async (proposalId: number): Promise<Proposal> => {
      return axios
        .get(`/proposal/${proposalId}`)
        .then((response: AxiosResponse<Proposal>): Proposal => response.data);
    },
  };
}
