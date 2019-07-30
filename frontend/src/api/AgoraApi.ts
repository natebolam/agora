import { AxiosInstance, AxiosResponse } from "axios";
import {
  ExplorationPeriodInfo,
  MetaPeriodInfo,
  PromotionPeriodInfo,
  Proposal,
  ProposalPeriodInfo,
  TestingPeriodInfo,
} from "~/models/Period";
import { ProposalsList } from "~/models/ProposalsList";
import { ProposalBallotsList } from "~/models/ProposalBallotsList";
import { ProposalVotesList } from "~/models/ProposalVotesList";
import { Decision } from "~/models/Decision";

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
  getBallots: (
    periodId: number,
    decision?: Decision,
    lastId?: number,
    limit?: number
  ) => Promise<ProposalBallotsList>;
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
    getBallots: async (
      periodId: number,
      decision?: Decision,
      lastId?: number,
      limit: number = 20
    ): Promise<ProposalBallotsList> => {
      return axios
        .get(`/ballots/${periodId}`, {
          params: {
            limit,
            lastId,
            decision,
          },
        })
        .then(
          (
            response: AxiosResponse<ProposalBallotsList>
          ): ProposalBallotsList => {
            return response.data;
          }
        );
    },
    getProposal: async (proposalId: number): Promise<Proposal> => {
      return axios
        .get(`/proposal/${proposalId}`)
        .then((response: AxiosResponse<Proposal>): Proposal => response.data);
    },
  };
}
