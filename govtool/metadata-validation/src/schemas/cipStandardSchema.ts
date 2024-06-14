import * as Joi from 'joi';

import { MetadataStandard } from '@types';

type StandardSpecification = Record<MetadataStandard, Joi.ObjectSchema<any>>;

const CIP100_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#';
const CIP108_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#';

// Temporary URL for the CIP-119 metadata
const CIPQQQ_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-QQQ/README.md#';

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
      title: Joi.object({
        '@value': Joi.string().max(80).required(),
      }).required(),
      abstract: Joi.object({
        '@value': Joi.string().max(2500).required(),
      }).required(),
      motivation: Joi.object({ '@value': Joi.string().required() }).required(),
      rationale: Joi.object({ '@value': Joi.string().required() }).required(),
      references: Joi.array()
        .items(
          Joi.object({
            '@type': Joi.string().required(),
            label: Joi.object({
              '@value': Joi.string().required(),
            }).required(),
            uri: Joi.object({
              '@value': Joi.string().required(),
            }).required(),
            referenceHash: Joi.object({
              hashDigest: Joi.string().required(),
              hashAlgorithm: Joi.string().required(),
            }),
          }),
        )
        .required(),
    }),
  }),
  [MetadataStandard.CIPQQQ]: Joi.object({
    '@context': Joi.object({
      '@language': Joi.string().required(),
      CIP100: Joi.string().valid(CIP100_URL).required(),
      CIPQQQ: Joi.string().valid(CIPQQQ_URL).required(),
      hashAlgorithm: Joi.string().valid('CIP100:hashAlgorithm').required(),
      body: Joi.object(),
      authors: Joi.object(),
    }),
    authors: Joi.array(),
    hashAlgorithm: Joi.object({
      '@value': Joi.string().valid('blake2b-256').required(),
    }),
    body: Joi.object({
      bio: Joi.object({ '@value': Joi.string().allow('') }).required(),
      dRepName: Joi.object({ '@value': Joi.string().allow('') }).required(),
      email: Joi.object({ '@value': Joi.string().allow('') }).required(),
      references: Joi.array()
        .items(
          Joi.object({
            '@type': Joi.string(),
            label: Joi.object({
              '@value': Joi.string().allow('').required(),
            }).required(),
            uri: Joi.object({
              '@value': Joi.string().required(),
            }).required(),
            referenceHash: Joi.object({
              hashDigest: Joi.string().required(),
              hashAlgorithm: Joi.string().required(),
            }),
          }),
        )
        .required(),
    }),
  }),
};
