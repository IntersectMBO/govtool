import * as blake from "blakejs";
import { isAxiosError } from "axios";

import { API } from "@services";
import {
  sharedGovernanceActionFields,
  MetadataHashValidationErrors,
} from "@consts";

import { URL_REGEX, areObjectsTheSame, canonizeJSON } from ".";

export enum GAMetedataErrors {
  DATA_MISSING = "Data Missing",
  NOT_VERIFIABLE = "Data Not Verifiable",
  INCORRECT_FORMAT = "Data Formatted Incorrectly",
}

/**
 * Validates the metadata hash by fetching the metadata from the given URL,
 * canonizing it, and comparing the hash with the provided hash.
 *
 * @param storingURL - The URL where the metadata is stored.
 * @param hash - The hash to compare with the calculated hash of the metadata.
 * @returns A promise that resolves to `true` if the metadata hash is valid,
 * or rejects with an error message if validation fails.
 */
export const validateMetadataHash = async (
  storingURL: string,
  hash: string,
) => {
  try {
    if (!storingURL.match(URL_REGEX)) {
      throw new Error(MetadataHashValidationErrors.INVALID_URL);
    }

    const { data: userMetadataJSON } = await API.get(storingURL);

    let canonizedUserMetadata;
    try {
      canonizedUserMetadata = await canonizeJSON(userMetadataJSON);
    } catch (error) {
      throw new Error(MetadataHashValidationErrors.INVALID_JSON);
    }
    if (!canonizedUserMetadata) {
      throw new Error(MetadataHashValidationErrors.INVALID_JSON);
    }

    const hashedUserMetadata = blake.blake2bHex(
      canonizedUserMetadata,
      undefined,
      32,
    );

    if (hashedUserMetadata !== hash) {
      throw new Error(MetadataHashValidationErrors.INVALID_HASH);
    }
    return true;
  } catch (error) {
    if (isAxiosError(error)) {
      throw new Error(MetadataHashValidationErrors.FETCH_ERROR);
    }
    throw error;
  }
};

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
