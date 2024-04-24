import { MetadataStandard } from '@/types';

const CIP_108_VALUE_KEYS = ['abstract', 'motivation', 'rationale', 'title'];
export const parseMetadata = (metadata: any, standard: MetadataStandard) => {
  const parsedMetadata = {};
  switch (standard) {
    case MetadataStandard.CIP108:
      for (const [key, value] of Object.entries(metadata)) {
        if (CIP_108_VALUE_KEYS.includes(key)) {
          parsedMetadata[key] = value['@value'];
        }

        if (key === 'references') {
          parsedMetadata[key] = (Array.isArray(value) ? value : [])?.map(
            (reference) => reference['CIP108:reference-uri']['@value'],
          );
        }
      }
      return parsedMetadata;
    default:
      return;
  }
};
