import axios, { AxiosInstance, AxiosRequestConfig } from "axios";
import { AgoraApi } from "./AgoraApi";

const axiosConfig: AxiosRequestConfig = {
  baseURL: "http://staging.agora.tezos.serokell.org:8190/api/v1",
};

const axiosIntance: AxiosInstance = axios.create(axiosConfig);

export const Api = {
  agoraApi: AgoraApi(axiosIntance),
};
