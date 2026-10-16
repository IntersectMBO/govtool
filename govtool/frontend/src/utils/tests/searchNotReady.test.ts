import { AxiosError, AxiosHeaders } from "axios";
import { describe, expect, it } from "vitest";

import { isSearchNotReady } from "../searchNotReady";

const axiosError = (status: number, data: unknown) =>
  new AxiosError("request failed", "ERR_BAD_RESPONSE", undefined, undefined, {
    status,
    statusText: "",
    data,
    headers: {},
    config: { headers: new AxiosHeaders() },
  });

describe("isSearchNotReady", () => {
  it("recognises the backend's search-not-ready answer", () => {
    expect(
      isSearchNotReady(
        axiosError(503, {
          errorType: "ServiceUnavailableError",
          message: "still fetching",
        }),
      ),
    ).toBe(true);
  });

  it("leaves every other failure alone", () => {
    expect(
      isSearchNotReady(
        axiosError(503, { errorType: "MetadataUnconfiguredError" }),
      ),
    ).toBe(false);
    expect(
      isSearchNotReady(
        axiosError(500, { errorType: "ServiceUnavailableError" }),
      ),
    ).toBe(false);
    expect(isSearchNotReady(new Error("network"))).toBe(false);
    expect(isSearchNotReady(undefined)).toBe(false);
  });
});
