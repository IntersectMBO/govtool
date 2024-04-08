import * as blake from "blakejs";

import { API } from "@services";
import { sharedGovernanceActionFields } from "@consts";

import { URL_REGEX, areObjectsTheSame, canonizeJSON } from ".";

export enum GAMetedataErrors {
  DATA_MISSING = "Data Missing",
  NOT_VERIFIABLE = "Data Not Verifiable",
  INCORRECT_FORMAT = "Data Formatted Incorrectly",
}

export const checkIsMissingGAMetadata = async ({
  url,
  hash,
}: {
  url: string;
  hash: string;
}): Promise<boolean | GAMetedataErrors> => {
  if (!url?.match(URL_REGEX)) {
    return GAMetedataErrors.DATA_MISSING;
  }

  let gaMetadata;
  try {
    const { data } = await API.get(url);
    gaMetadata = data;
  } catch (e) {
    return GAMetedataErrors.DATA_MISSING;
  }
  const JSONBody = gaMetadata?.body;

  if (!JSONBody) {
    return GAMetedataErrors.DATA_MISSING;
  }

  const govtoolFields = {
    ...sharedGovernanceActionFields,
    references: [],
  };

  if (!areObjectsTheSame(JSONBody, govtoolFields)) {
    return GAMetedataErrors.INCORRECT_FORMAT;
  }

  let canonizedGAMetadata;
  try {
    canonizedGAMetadata = await canonizeJSON(gaMetadata);
  } catch (error) {
    return GAMetedataErrors.INCORRECT_FORMAT;
  }

  const gaHash = blake.blake2bHex(canonizedGAMetadata, undefined, 32);

  if (gaHash !== hash) {
    return GAMetedataErrors.NOT_VERIFIABLE;
  }

  return false;
};
