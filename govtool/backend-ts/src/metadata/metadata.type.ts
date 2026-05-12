import { MetadataValidationStatus } from "./metadata-status.enum";

export enum MetadataStandard {
    CIP108 = 'CIP108',
    CIP119 = 'CIP119',
}

export type ValidateMetadataResult = {
    status?: MetadataValidationStatus;
    valid: boolean;
    metadata?: Record<string,unknown>;
};
