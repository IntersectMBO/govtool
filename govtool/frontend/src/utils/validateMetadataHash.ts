import { postValidate } from "@services";

import { MetadataStandard, MetadataValidationStatus } from "@/models";

export const checkIsMissingGAMetadata = async ({
  url,
  hash,
}: {
  url: string;
  hash: string;
}): Promise<boolean | MetadataValidationStatus> => {
  try {
    const { status } = await postValidate({
      url,
      hash,
      standard: MetadataStandard.CIP108,
    });
    if (status) {
      return status;
    }
    return false;
  } catch (error) {
    return MetadataValidationStatus.URL_NOT_FOUND;
  }
};
