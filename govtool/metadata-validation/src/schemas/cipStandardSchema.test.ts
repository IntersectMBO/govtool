import { MetadataStandard } from '@types';

import { cipStandardSchema } from './cipStandardSchema';

const validCIP108Data = {
  '@context': {
    CIP100:
      'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#',
    CIP108:
      'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#',
    hashAlgorithm: 'CIP100:hashAlgorithm',
    body: {},
    authors: {},
  },
  authors: [],
  hashAlgorithm: {
    '@value': 'blake2b-256',
  },
  body: {
    title: { '@value': 'Sample Title' },
    abstract: { '@value': 'Sample Abstract' },
    motivation: { '@value': 'Sample Motivation' },
    rationale: { '@value': 'Sample Rationale' },
    references: [
      {
        '@type': 'ReferenceType',
        label: { '@value': 'Sample Label' },
        uri: { '@value': 'https://sampleuri.com' },
        referenceHash: {
          hashDigest: 'sampleHashDigest',
          hashAlgorithm: 'sampleHashAlgorithm',
        },
      },
    ],
  },
};

const validCIPQQQData = {
  '@context': {
    CIP100:
      'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#',
    CIPQQQ:
      'https://github.com/cardano-foundation/CIPs/blob/master/CIP-QQQ/README.md#',
    hashAlgorithm: 'CIP100:hashAlgorithm',
    body: {},
    authors: {},
  },
  authors: [],
  hashAlgorithm: {
    '@value': 'blake2b-256',
  },
  body: {
    bio: { '@value': 'Sample Bio' },
    dRepName: { '@value': 'Sample Name' },
    email: { '@value': 'sample@example.com' },
    references: [
      {
        '@type': 'ReferenceType',
        label: { '@value': 'Sample Label' },
        uri: { '@value': 'https://sampleuri.com' },
        referenceHash: {
          hashDigest: 'sampleHashDigest',
          hashAlgorithm: 'sampleHashAlgorithm',
        },
      },
    ],
  },
};

describe('cipStandardSchema', () => {
  it('should validate CIP108 data correctly', () => {
    const { error } =
      cipStandardSchema[MetadataStandard.CIP108].validate(validCIP108Data);
    expect(error).toBeUndefined();
  });

  it('should invalidate CIP108 data with missing required fields', () => {
    const invalidData = {
      ...validCIP108Data,
      body: { ...validCIP108Data.body, title: {} },
    };
    const { error } =
      cipStandardSchema[MetadataStandard.CIP108].validate(invalidData);
    expect(error).toBeDefined();
  });

  it('should invalidate CIP108 data with wrong hashAlgorithm', () => {
    const invalidData = {
      ...validCIP108Data,
      hashAlgorithm: {
        '@value': 'wrongHashAlgorithm',
      },
    };
    const { error } =
      cipStandardSchema[MetadataStandard.CIP108].validate(invalidData);
    expect(error).toBeDefined();
  });

  it('should invalidate CIP108 with too long title', () => {
    const invalidData = {
      ...validCIP108Data,
      body: {
        ...validCIP108Data.body,
        title: {
          '@value': 'a'.repeat(81),
        },
      },
    };
    const { error } =
      cipStandardSchema[MetadataStandard.CIP108].validate(invalidData);
    expect(error).toBeDefined();
  });

  it('should invalidate CIP108 data with wrong references', () => {
    const invalidData = {
      ...validCIP108Data,
      body: {
        ...validCIP108Data.body,
        references: [
          {
            '@type': 'ReferenceType',
            label: { '@value': 'Sample Label' },
            // Missing uri
            referenceHash: {
              hashDigest: 'sampleHashDigest',
              hashAlgorithm: 'sampleHashAlgorithm',
            },
          },
        ],
      },
    };
    const { error } =
      cipStandardSchema[MetadataStandard.CIP108].validate(invalidData);
    expect(error).toBeDefined();
  });

  it('should validate CIP108 with empty references', () => {
    const { error } = cipStandardSchema[MetadataStandard.CIP108].validate({
      ...validCIP108Data,
      body: { ...validCIP108Data.body, references: [] },
    });
    expect(error).toBeUndefined();
  });

  it('should validate CIPQQQ data correctly', () => {
    const { error } =
      cipStandardSchema[MetadataStandard.CIPQQQ].validate(validCIPQQQData);
    expect(error).toBeUndefined();
  });

  it('should invalidate CIPQQQ data with missing required fields', () => {
    const invalidData = {
      ...validCIPQQQData,
      body: { ...validCIPQQQData.body, bio: undefined },
    };
    const { error } =
      cipStandardSchema[MetadataStandard.CIPQQQ].validate(invalidData);
    expect(error).toBeDefined();
  });

  it('should invalidate CIPQQQ data with wrong references', () => {
    const invalidData = {
      ...validCIPQQQData,
      body: {
        ...validCIPQQQData.body,
        references: [
          {
            '@type': 'ReferenceType',
            label: { '@value': 'Sample Label' },
            // Missing uri
            referenceHash: {
              hashDigest: 'sampleHashDigest',
              hashAlgorithm: 'sampleHashAlgorithm',
            },
          },
        ],
      },
    };
    const { error } =
      cipStandardSchema[MetadataStandard.CIPQQQ].validate(invalidData);
    expect(error).toBeDefined();
  });
});
