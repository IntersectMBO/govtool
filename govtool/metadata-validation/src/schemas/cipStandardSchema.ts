import * as Joi from 'joi';

import { MetadataStandard } from '@types';

type StandardSpecification = Record<MetadataStandard, Joi.ObjectSchema<any>>;

const CIP100_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#';
const CIP108_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#';
const CIP119_URL =
  'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#';

export const cipStandardSchema: StandardSpecification = {
  // Source of CIP-108: https://github.com/Ryun1/CIPs/blob/governance-metadata-actions/CIP-0108/README.md
  [MetadataStandard.CIP108]: Joi.object({
    '@context': Joi.object({
      CIP100: Joi.string().valid(CIP100_URL).required(),
      CIP108: Joi.string().valid(CIP108_URL).required(),
      hashAlgorithm: Joi.string().valid('CIP100:hashAlgorithm').required(),
      body: Joi.object(),
      authors: Joi.object(),
    }),
    authors: Joi.array(),
    hashAlgorithm: Joi.string().valid('blake2b-256').required(),
    body: Joi.object({
      title: Joi.string().max(80).required(),
      abstract: Joi.string().max(2500).required(),
      motivation: Joi.string().required(),
      rationale: Joi.string().required(),
      references: Joi.array()
        .items(
          Joi.object({
            '@type': Joi.string().required(),
            label: Joi.string().required(),
            uri: Joi.string().required(),
            referenceHash: Joi.object({
              hashDigest: Joi.string().required(),
              hashAlgorithm: Joi.string().required(),
            }),
          }),
        )
        .required(),
    }),
  }),
  [MetadataStandard.CIP119]: Joi.object({
    '@context': Joi.object({
      CIP100: Joi.string().valid(CIP100_URL).required(),
      CIP119: Joi.string().valid(CIP119_URL).required(),
      hashAlgorithm: Joi.string().valid('CIP100:hashAlgorithm').required(),
      body: Joi.object(),
      authors: Joi.object(),
    }),
    authors: Joi.array(),
    hashAlgorithm: Joi.string().valid('blake2b-256').required(),
    body: Joi.object({
      paymentAddress: Joi.string().allow(''),
      givenName: Joi.string().allow('').required(),
      image: Joi.any(),
      objectives: Joi.string().max(1000).allow(''),
      motivations: Joi.string().max(1000).allow(''),
      qualifications: Joi.string().max(1000).allow(''),
      doNotList: Joi.boolean(),
      references: Joi.array()
        .items(
          Joi.object({
            '@type': Joi.string(),
            label: Joi.string().allow('').required(),
            uri: Joi.string().required(),
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
