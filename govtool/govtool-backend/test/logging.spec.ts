import { Test } from '@nestjs/testing';
import {
  Controller,
  Get,
  INestApplication,
  Logger,
  NotFoundException,
  ServiceUnavailableException,
} from '@nestjs/common';
import { APP_FILTER } from '@nestjs/core';
import request from 'supertest';
import type { Server } from 'node:http';
import { LoggingExceptionFilter } from '../src/common/logging-exception.filter';

@Controller('boom')
class BoomController {
  @Get('unknown')
  unknown(): never {
    throw new Error('unexpected failure');
  }

  @Get('unavailable')
  unavailable(): never {
    throw new ServiceUnavailableException({
      errorType: 'PinataAPIError',
      message: 'upstream down',
    });
  }

  @Get('missing')
  missing(): never {
    throw new NotFoundException();
  }
}

describe('Error logging (#2776)', () => {
  let app: INestApplication<Server>;
  let errorSpy: jest.SpyInstance;

  beforeAll(async () => {
    const module = await Test.createTestingModule({
      controllers: [BoomController],
      providers: [{ provide: APP_FILTER, useClass: LoggingExceptionFilter }],
    }).compile();
    app = module.createNestApplication<INestApplication<Server>>({
      logger: false,
    });
    await app.init();
  });

  beforeEach(() => {
    errorSpy = jest
      .spyOn(Logger.prototype, 'error')
      .mockImplementation(() => undefined);
  });

  afterEach(() => {
    errorSpy.mockRestore();
  });

  afterAll(async () => {
    await app.close();
  });

  it('logs unexpected errors and returns 500', async () => {
    await request(app.getHttpServer()).get('/boom/unknown').expect(500);
    expect(errorSpy).toHaveBeenCalled();
    const logged = errorSpy.mock.calls
      .flat()
      .map((arg: unknown) => (arg instanceof Error ? arg.message : String(arg)))
      .join(' ');
    expect(logged).toContain('unexpected failure');
  });

  it('logs 5xx HttpExceptions with method, path and payload', async () => {
    const response = await request(app.getHttpServer())
      .get('/boom/unavailable')
      .expect(503);
    expect(response.body).toMatchObject({ errorType: 'PinataAPIError' });
    expect(errorSpy).toHaveBeenCalledTimes(1);
    const [message] = errorSpy.mock.calls[0] as [string];
    expect(message).toContain('GET /boom/unavailable responded 503');
    expect(message).toContain('PinataAPIError');
  });

  it('does not log 4xx client errors', async () => {
    await request(app.getHttpServer()).get('/boom/missing').expect(404);
    expect(errorSpy).not.toHaveBeenCalled();
  });
});
