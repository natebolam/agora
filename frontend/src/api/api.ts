import axios, { AxiosInstance, AxiosRequestConfig } from "axios";
import { AgoraApi } from "./AgoraApi";

const axiosConfig: AxiosRequestConfig = {
  baseURL: "https://staging.agora.tezos.serokell.org/api/v1",
};

const axiosIntance: AxiosInstance = axios.create(axiosConfig);

export const Api = {
  agoraApi: AgoraApi(axiosIntance),
};
