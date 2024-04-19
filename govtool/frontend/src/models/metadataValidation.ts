// TODO: Should be taken from @govtool/metadata-validation
export enum MetadataValidationStatus {
  URL_NOT_FOUND = "URL_NOT_FOUND",
  INVALID_JSONLD = "INVALID_JSONLD",
  INVALID_HASH = "INVALID_HASH",
}

export type ValidateMetadataResult = {
  status?: MetadataValidationStatus;
  valid: boolean;
};

export enum MetadataStandard {
  CIP108 = "CIP108",
}

export type MetadataValidationDTO = {
  url: string;
  hash: string;
  standard?: MetadataStandard;
};
