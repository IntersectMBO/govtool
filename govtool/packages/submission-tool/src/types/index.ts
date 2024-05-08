export enum CIP_Reference {
  "0100" = "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#",
  "0108" = "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#",
}

export type MetadataReference = {
  label: string;
  uri: string;
};

export type MetadataBody = {
  title: string;
  abstract: string;
  motivation: string;
  rationale: string;
  references: MetadataReference[];
};

export type MetadataConfig = {
  cip: CIP_Reference;
  body: MetadataBody;
  hashAlgorithm: string;
};
