export type SurveyDefinitionRow = {
  payload_cbor_hex: string;
};

export type SurveyDefinitionResponse = {
  txId: string;
  surveyIndex: number;
  metadataLabel: 17;
  payloadCborHex: string;
};
