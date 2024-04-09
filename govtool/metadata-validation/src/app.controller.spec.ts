import { Test, TestingModule } from '@nestjs/testing';
import { HttpModule } from '@nestjs/axios';

import { MetadataValidationStatus } from '@enums';

import { AppController } from './app.controller';
import { AppService } from './app.service';

// TODO: Mock HttpService
describe('AppController', () => {
  let appController: AppController;

  beforeEach(async () => {
    const app: TestingModule = await Test.createTestingModule({
      imports: [
        HttpModule.register({
          timeout: 5000,
          maxRedirects: 5,
        }),
      ],
      controllers: [AppController],
      providers: [AppService],
    }).compile();

    appController = app.get<AppController>(AppController);
  });

  it('should throw invalid URL', async () => {
    const result = await appController.validateMetadata({
      hash: 'hash',
      url: 'url',
    });
    expect(result).toEqual({
      status: MetadataValidationStatus.URL_NOT_FOUND,
      valid: false,
    });
  });

  it('should throw invalid JSONLD', async () => {
    const result = await appController.validateMetadata({
      hash: 'hash',
      url: 'http://www.schema.org',
    });

    expect(result).toEqual({
      status: MetadataValidationStatus.INVALID_JSONLD,
      valid: false,
    });
  });
});
