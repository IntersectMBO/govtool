import { Logger } from '@nestjs/common';
import { LoggerMessage } from '@/enums';
import { MetadataStandard } from '@/types';

const CIP_108_VALUE_KEYS = ['abstract', 'motivation', 'rationale', 'title'];
const CIP_QQQ_VALUE_KEYS = ['bio', 'dRepName', 'email', 'references'];
export const parseMetadata = (
  metadata: any,
  standard = MetadataStandard.CIP108,
) => {
  const parsedMetadata = {};
  switch (standard) {
    case MetadataStandard.CIP108:
      for (const [key, value] of Object.entries(metadata)) {
        if (CIP_108_VALUE_KEYS.includes(key)) {
          parsedMetadata[key] = value;
        }

        if (key === 'references') {
          parsedMetadata[key] = (Array.isArray(value) ? value : [])?.map(
            (reference) => reference?.uri,
          );
        }
      }

      return parsedMetadata;

    case MetadataStandard.CIPQQQ:
      console.log({ metadataRaw: metadata });
      for (const [key, value] of Object.entries(metadata)) {
        if (CIP_QQQ_VALUE_KEYS.includes(key)) {
          parsedMetadata[key] = value;
        }
        if (key === 'references') {
          parsedMetadata[key] = (Array.isArray(value) ? value : [])?.map(
            (reference) => reference?.uri,
          );
        }
      }
      return parsedMetadata;
    default:
      Logger.warn(LoggerMessage.CANNOT_PARSE_METADATA_BODY, { standard });
      return;
  }
};
