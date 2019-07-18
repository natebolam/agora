import { AxiosInstance, AxiosResponse } from "axios";

interface GetTestResponse {
  test: number;
}

interface TestApiType {
  getTest: (value: number) => Promise<GetTestResponse>;
}

export function TestApi(axios: AxiosInstance): TestApiType {
  async function getTest(value: number): Promise<GetTestResponse> {
    return axios
      .get("/test", {
        params: {
          value,
        },
      })
      .then(
        (response: AxiosResponse<GetTestResponse>): GetTestResponse => {
          return response.data;
        }
      );
  }

  return {
    getTest,
  };
}
