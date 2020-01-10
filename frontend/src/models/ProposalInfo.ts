export interface Url {
  description: string;
  hash: string;
  url: string;
}

export type Urls = Url[];

export interface Proposal {
  id: number;
  stage: number;
  hash: string;
  title: string;
  urls: Urls;
  shortDescription: string;
  longDescription: string;
  timeCreated: string;
  discourseLink: string;
  votesCasted: number;
}

export type ProposalsList = Proposal[];
