// Typescript checking is not crucial for this unit tests, so its easier to disable it
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
/* eslint-disable @typescript-eslint/no-explicit-any */
import { vi } from "vitest";
import { setProtocolParameterUpdate } from "../setProtocolParameterUpdate";

describe("setProtocolParameterUpdate", () => {
  it("should call the corresponding setter function if the value is not undefined", () => {
    const protocolParameterUpdate: any = {
      set_key: vi.fn() as (value: unknown) => void,
    };

    setProtocolParameterUpdate(protocolParameterUpdate, "key", "value");

    expect(protocolParameterUpdate.set_key).toHaveBeenCalledWith("value");
  });

  it("should not call any setter function if the value is undefined", () => {
    const protocolParameterUpdate: any = {
      set_key: vi.fn() as (value: unknown) => void,
    };

    setProtocolParameterUpdate(protocolParameterUpdate, "key", undefined);

    expect(protocolParameterUpdate.set_key).not.toHaveBeenCalled();
  });

  it("should handle snake case keys correctly", () => {
    const protocolParameterUpdate: any = {
      set_snake_case_key: vi.fn() as (value: unknown) => void,
    };

    setProtocolParameterUpdate(
      protocolParameterUpdate,
      "snakeCaseKey",
      "value",
    );

    expect(protocolParameterUpdate.set_snake_case_key).toHaveBeenCalledWith(
      "value",
    );
  });

  it("should not call any setter function if the corresponding setter does not exist", () => {
    const protocolParameterUpdate: any = {};

    setProtocolParameterUpdate(protocolParameterUpdate, "key", "value");

    expect(protocolParameterUpdate.set_key).toBeUndefined();
  });
});
