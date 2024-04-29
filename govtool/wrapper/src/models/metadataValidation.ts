// TODO: Should be taken from @govtool/metadata-validation
export enum MetadataValidationStatus {
  URL_NOT_FOUND = "URL_NOT_FOUND",
  INVALID_JSONLD = "INVALID_JSONLD",
  INVALID_HASH = "INVALID_HASH",
  INCORRECT_FORMAT = "INCORRECT_FORMAT",
}

export type ValidateMetadataResult = {
  status?: MetadataValidationStatus;
  valid: boolean;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  metadata?: any;
};

export enum MetadataStandard {
  CIP108 = "CIP108",
}

export type MetadataValidationDTO = {
  url: string;
  hash: string;
  standard?: MetadataStandard;
};
