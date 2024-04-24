import * as Joi from 'joi';

import { MetadataStandard } from '@types';

type StandardSpecification = Record<MetadataStandard, Joi.ObjectSchema<any>>;

const CIP100_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#';
const CIP108_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#';

export const cipStandardSchema: StandardSpecification = {
  // Source of CIP-108: https://github.com/Ryun1/CIPs/blob/governance-metadata-actions/CIP-0108/README.md
  [MetadataStandard.CIP108]: Joi.object({
    '@context': Joi.object({
      '@language': Joi.string().required(),
      CIP100: Joi.string().valid(CIP100_URL).required(),
      CIP108: Joi.string().valid(CIP108_URL).required(),
      hashAlgorithm: Joi.string().valid('CIP100:hashAlgorithm').required(),
      body: Joi.object(),
      authors: Joi.object(),
    }),
    authors: Joi.array(),
    hashAlgorithm: Joi.object({
      '@value': Joi.string().valid('blake2b-256').required(),
    }),
    body: Joi.object({
      title: Joi.object({ '@value': Joi.string().max(80).required() }),
      abstract: Joi.object({ '@value': Joi.string().max(2500).required() }),
      motivation: Joi.object({ '@value': Joi.string().required() }),
      rationale: Joi.object({ '@value': Joi.string().required() }),
      references: Joi.array().items(
        Joi.object({
          '@type': Joi.string(),
          'CIP108:reference-label': Joi.object({
            '@value': Joi.string().required(),
          }),
          'CIP108:reference-uri': Joi.object({
            '@value': Joi.string().uri().required(),
          }),
          'CIP108:reference-hash': Joi.object({
            hashDigest: Joi.string().required(),
            hashAlgorithm: Joi.string().required(),
          }),
        }).required(),
      ),
    }),
  }),
};
