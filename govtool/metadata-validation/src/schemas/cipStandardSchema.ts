import * as Joi from 'joi';

import { MetadataStandard } from '@types';

type StandardSpecification = Record<MetadataStandard, Joi.ObjectSchema<any>>;

const CIP100body = Joi.object({
  references: Joi.array().items(
    Joi.object({
      '@type': Joi.string().required(),
      label: Joi.string().required(),
      uri: Joi.string().required(),
      referenceHash: Joi.object({
        hashDigest: Joi.string().required(),
        hashAlgorithm: Joi.string().required(),
      }),
    }),
  ),
  comment: Joi.string().allow(''),
  externalUpdates: Joi.array(),
});

const CIP108body = {
  title: Joi.string().max(80),
  abstract: Joi.string().max(2500),
  motivation: Joi.string(),
  rationale: Joi.string(),
};

const CIP119body = {
  paymentAddress: Joi.string().allow(''),
  givenName: Joi.string().max(80).allow('').required(),
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
};

export const cipStandardSchema: StandardSpecification = {
  [MetadataStandard.CIP108]: Joi.object({
    '@context': Joi.object(),
    authors: Joi.array(),
    hashAlgorithm: Joi.string().valid('blake2b-256').required(),
    body: CIP100body.append(CIP108body),
  }),
  [MetadataStandard.CIP119]: Joi.object({
    '@context': Joi.object(),
    authors: Joi.array(),
    hashAlgorithm: Joi.string().valid('blake2b-256').required(),
    // TODO: Declare the Person schema https://schema.org/Person
    // Append empty object to allow for optional fields extended from Person schema
    body: CIP100body.append(CIP108body).append(CIP119body).append({}),
  }),
};
