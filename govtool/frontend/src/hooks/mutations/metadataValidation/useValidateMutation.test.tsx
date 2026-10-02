import { ReactNode } from "react";
import { renderHook } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import { MetadataValidationStatus } from "@models";

import { useValidateMutation } from "./useValidateMutation";

const mocks = vi.hoisted(() => ({ postValidate: vi.fn() }));
vi.mock("@services", () => ({ postValidate: mocks.postValidate }));

const render = () => {
  const client = new QueryClient();
  const wrapper = ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={client}>{children}</QueryClientProvider>
  );
  return renderHook(() => useValidateMutation(), { wrapper });
};

const body = { url: "https://example.org/a.jsonld", hash: "ab".repeat(32) };

beforeEach(() => vi.clearAllMocks());

describe("useValidateMutation", () => {
  it("returns the backend's answer", async () => {
    mocks.postValidate.mockResolvedValue({ valid: true, metadata: {} });
    const hook = render();
    await expect(hook.result.current.validateMetadata(body)).resolves.toEqual({
      valid: true,
      metadata: {},
    });
  });

  it("resolves a failed request as INTERNAL_ERROR instead of rejecting", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    mocks.postValidate.mockRejectedValue(new Error("timeout of 30000ms"));
    const hook = render();
    await expect(hook.result.current.validateMetadata(body)).resolves.toEqual({
      valid: false,
      status: MetadataValidationStatus.INTERNAL_ERROR,
    });
  });

  it("does not answer a submission check from a read's cached result", async () => {
    mocks.postValidate
      .mockResolvedValueOnce({ valid: true })
      .mockResolvedValueOnce({
        valid: false,
        status: MetadataValidationStatus.URL_NOT_FOUND,
      });
    const hook = render();
    await hook.result.current.validateMetadata(body);
    await expect(
      hook.result.current.validateMetadata({ ...body, verifyUrl: true }),
    ).resolves.toMatchObject({
      status: MetadataValidationStatus.URL_NOT_FOUND,
    });
    expect(mocks.postValidate).toHaveBeenLastCalledWith({
      ...body,
      verifyUrl: true,
    });
  });
});
