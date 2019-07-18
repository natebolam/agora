import axios, { AxiosInstance, AxiosRequestConfig } from "axios";
import { TestApi } from "~/api/TestApi";

const axiosConfig: AxiosRequestConfig = {
  baseURL: "https://example.com",
};

const axiosIntance: AxiosInstance = axios.create(axiosConfig);

export const TezosApi = {
  testApi: TestApi(axiosIntance),
};
