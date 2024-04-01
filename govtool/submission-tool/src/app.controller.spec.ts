import { Test, TestingModule } from '@nestjs/testing';
import { HttpModule } from '@nestjs/axios';
import { HttpException, HttpStatus } from '@nestjs/common';

import { AppController } from './app.controller';
import { AppService } from './app.service';
import { ValidationError } from './enums/ValidationError';

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

  describe('metadata validation', () => {
    it('should throw invalid URL', async () => {
      try {
        await appController.validateMetadata({
          hash: 'hash',
          url: 'url',
        });
      } catch (error) {
        expect(error).toBeInstanceOf(HttpException);
        expect((error as HttpException).getStatus()).toBe(
          HttpStatus.BAD_REQUEST,
        );
        expect((error as HttpException).getResponse()).toEqual(
          ValidationError.URL_NOT_FOUND,
        );
      }
    });

    it('should throw invalid JSONLD', async () => {
      try {
        await appController.validateMetadata({
          hash: 'hash',
          url: 'http://www.schema.org',
        });
      } catch (error) {
        expect(error).toBeInstanceOf(HttpException);
        expect((error as HttpException).getStatus()).toBe(
          HttpStatus.BAD_REQUEST,
        );
        expect((error as HttpException).getResponse()).toEqual(
          ValidationError.INVALID_JSONLD,
        );
      }
    });
  });
});
