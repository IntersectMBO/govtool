import { postValidate } from "@services";

import { MetadataValidationStatus } from "@/models";

type CheckIsMissingGAMetadataResponse = {
  status?: MetadataValidationStatus;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  metadata?: any;
  valid: boolean;
};

export const checkIsMissingGAMetadata = async ({
  url,
  hash,
}: {
  url: string;
  hash: string;
}): Promise<CheckIsMissingGAMetadataResponse> => {
  try {
    const { status, metadata, valid } = await postValidate({
      url,
      hash,
    });
    if (status) {
      return { status, valid };
    }
    return { metadata, valid };
  } catch (error) {
    return { status: MetadataValidationStatus.URL_NOT_FOUND, valid: false };
  }
};
