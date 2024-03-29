import { Test, TestingModule } from '@nestjs/testing';
import { AppController } from './app.controller';
import { AppService } from './app.service';

describe('AppController', () => {
  let appController: AppController;

  beforeEach(async () => {
    const app: TestingModule = await Test.createTestingModule({
      controllers: [AppController],
      providers: [AppService],
    }).compile();

    appController = app.get<AppController>(AppController);
  });

  describe('metadata validation', () => {
    it('should return hash and url', () => {
      expect(
        appController.validateMetadata({ hash: '', url: '' }),
      ).toStrictEqual({
        hash: '',
        url: '',
      });
    });
  });
});
