enum MetadataValidationStatus {
  URL_NOT_FOUND = "URL_NOT_FOUND",
  INVALID_JSONLD = "INVALID_JSONLD",
  INVALID_HASH = "INVALID_HASH",
  INCORRECT_FORMAT = "INCORRECT_FORMAT",
  EXCEEDS_LIMIT = "EXCEEDS_LIMIT",
}
declare module "@intersect.mbo/govtool-outcomes-pillar-ui/dist/esm" {
  type GovernanceActionsOutcomesProps = {
    apiUrl?: string;
    ipfsGateway?: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    walletAPI?: any;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    i18n?: any;
  };

  export default function GovernanceActionsOutcomes(
    props: GovernanceActionsOutcomesProps,
  ): JSX.Element;
}
