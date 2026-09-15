import { postValidate } from "./metadataValidation";
import { MetadataStandard } from "@/models";

export const getVoteContextTextFromFile = async (url: string | undefined,
          contextHash : string | undefined) => {
  if (!url || !contextHash) {
    throw new Error("Missing Vote Context values");
  }
  const response = await postValidate({
    standard: MetadataStandard.CIP100,
    url,
    hash: contextHash
  });

  return { valid: response.valid, metadata: response.metadata };
};
