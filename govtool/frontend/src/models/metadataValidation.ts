// TODO: Should be taken from @govtool/metadata-validation
export enum MetadataValidationStatus {
  URL_NOT_FOUND = "URL_NOT_FOUND",
  INVALID_JSONLD = "INVALID_JSONLD",
  INVALID_HASH = "INVALID_HASH",
  INCORRECT_FORMAT = "INCORRECT_FORMAT",
}

export enum MetadataStandard {
  CIP108 = "CIP108",
  CIPQQQ = "CIPQQQ",
}

export type ValidateMetadataResult<MetadataType> = {
  status?: MetadataValidationStatus;
  valid: boolean;
  metadata?: MetadataType;
};

export type MetadataValidationDTO = {
  url: string;
  hash: string;
  standard?: MetadataStandard;
};

export type DRepMetadata = {
  bio?: string;
  dRepName?: string;
  email?: string;
  references?: string[];
};

export type ProposalMetadata = {
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references?: string[];
  title?: string;
}
