import { isSameDRep } from "..";

import { DRepData, DRepStatus } from "@/models";

type TDRepType = "DRep" | "SoleVoter";

const EXAMPLE_DREP: DRepData = {
  drepId: "drep123",
  view: "view123",
  url: "url",
  metadataHash: "hash",
  deposit: 10000,
  votingPower: 10000,
  status: DRepStatus.Active,
  type: "DRep" as TDRepType,
  givenName: "name",
  identityReferences: [],
  linkReferences: [],
  latestRegistrationDate: "2024-07-10",
  paymentAddress: null,
  objectives: null,
  motivations: null,
  qualifications: null,
  doNotList: false,
  isScriptBased: false,
  imageUrl: null,
  image: null,
};

describe("isSameDRep function", () => {
  it("returns false if dRepIdOrView is undefined", () => {
    const dRepIdOrView = undefined;
    expect(isSameDRep(EXAMPLE_DREP, dRepIdOrView)).toBe(false);
  });

  it("returns true if drepId matches dRepIdOrView", () => {
    const dRepIdOrView = "drep123";
    expect(isSameDRep(EXAMPLE_DREP, dRepIdOrView)).toBe(true);
  });

  it("returns true if view matches dRepIdOrView", () => {
    const dRepIdOrView = "view123";
    expect(isSameDRep(EXAMPLE_DREP, dRepIdOrView)).toBe(true);
  });

  it("returns false if neither drepId nor view matches dRepIdOrView", () => {
    const dRepIdOrView = "otherId";
    expect(isSameDRep(EXAMPLE_DREP, dRepIdOrView)).toBe(false);
  });
});
