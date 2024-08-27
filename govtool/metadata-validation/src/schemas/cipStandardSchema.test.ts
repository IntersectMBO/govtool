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
  hashAlgorithm: 'blake2b-256',
  body: {
    title: 'Sample Title',
    abstract: 'Sample Abstract',
    motivation: 'Sample Motivation',
    rationale: 'Sample Rationale',
    references: [
      {
        '@type': 'ReferenceType',
        label: 'Sample Label',
        uri: 'https://sampleuri.com',
        referenceHash: {
          hashDigest: 'sampleHashDigest',
          hashAlgorithm: 'sampleHashAlgorithm',
        },
      },
    ],
  },
};

const validCIP119Data = {
  '@context': {
    CIP100:
      'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#',
    CIP119:
      'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#',
    hashAlgorithm: 'CIP100:hashAlgorithm',
    body: {},
    authors: {},
  },
  authors: [],
  hashAlgorithm: 'blake2b-256',
  body: {
    paymentAddress:
      'addr1qy49kr45ue0wq78d34dpg79syx3yekxryjadv9ykzczhjwm09pmyt6f6xvq5x9yah2vrxyg0np44ynm6n7hzafl2rqxs4v6nn3',
    givenName: 'Sample Given Name',
    image: {
      '@type': 'ImageObject',
      contentUrl: 'https://avatars.githubusercontent.com/u/24510246?v=4',
      sha256:
        '88c493a73b70627f5dd9c89fbb4335606ab635b2db56b86ac9071c2e59ccbac0',
    },
    objectives: 'Sample Objectives',
    motivations: 'Sample Motivations',
    qualifications: 'Sample Qualifications',
    doNotList: false,
    references: [
      {
        '@type': 'ReferenceType',
        label: 'Sample Label',
        uri: 'https://sampleuri.com',
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

  it('should validate CIP119 data correctly', () => {
    const { error } =
      cipStandardSchema[MetadataStandard.CIP119].validate(validCIP119Data);
    expect(error).toBeUndefined();
  });

  it('should invalidate CIP119 data with missing required fields', () => {
    const invalidData = {
      ...validCIP119Data,
      body: { ...validCIP119Data.body, givenName: 12312 },
    };
    const { error } =
      cipStandardSchema[MetadataStandard.CIP119].validate(invalidData);
    expect(error).toBeDefined();
  });

  it('should invalidate CIP119 data with wrong references', () => {
    const invalidData = {
      ...validCIP119Data,
      body: {
        ...validCIP119Data.body,
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
      cipStandardSchema[MetadataStandard.CIP119].validate(invalidData);
    expect(error).toBeDefined();
  });
});
