import { MetadataStandard } from '@types';

import { validateMetadataStandard } from './validateMetadataStandard';

describe('validateMetadataStandard', () => {
  it('should throw MetadataValidationStatus.INCORRECT_FORMAT if validation fails', async () => {
    try {
      await validateMetadataStandard(
        {
          testValue: 'test',
          anotherValue: 'another',
        },
        MetadataStandard.CIP108,
      );
    } catch (error) {
      expect(error).toBe('INCORRECT_FORMAT');
    }
  });
});
