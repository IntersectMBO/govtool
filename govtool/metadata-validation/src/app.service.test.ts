import { Test, TestingModule } from '@nestjs/testing';
import { HttpService } from '@nestjs/axios';
import { of, throwError } from 'rxjs';
import * as blake from 'blakejs';
import { lookup } from 'node:dns/promises';

import { AppService } from './app.service';
import { ValidateMetadataDTO } from '@dto';
import { MetadataValidationStatus } from '@enums';
import { MetadataStandard } from '@types';
import { validateMetadataStandard, parseMetadata, getStandard } from '@utils';
import { AxiosResponse, AxiosRequestHeaders } from 'axios';

jest.mock('@utils');
jest.mock('node:dns/promises', () => ({
  lookup: jest.fn(),
}));

describe('AppService', () => {
  let service: AppService;
  let httpService: HttpService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        AppService,
        {
          provide: HttpService,
          useValue: {
            get: jest.fn(),
          },
        },
      ],
    }).compile();

    service = module.get<AppService>(AppService);
    httpService = module.get<HttpService>(HttpService);
    (lookup as jest.Mock).mockResolvedValue([{ address: '93.184.216.34' }]);
  });

  it('should validate metadata correctly', async () => {
    const url = 'http://example.com';
    const hash = 'correctHash';
    const validateMetadataDTO: ValidateMetadataDTO = { hash, url };
    const body = {
      body: 'testBody',
      headers: {},
    };
    const parsedMetadata = { parsed: 'metadata' };
    const response: AxiosResponse = {
      data: JSON.stringify(body),
      status: 200,
      statusText: 'OK',
      headers: {},
      config: {
        headers: {} as AxiosRequestHeaders,
        url,
      },
    };
    jest.spyOn(httpService, 'get').mockReturnValueOnce(of(response));
    (getStandard as jest.Mock).mockReturnValueOnce(MetadataStandard.CIP108);
    (validateMetadataStandard as jest.Mock).mockResolvedValueOnce(undefined);
    (parseMetadata as jest.Mock).mockReturnValueOnce(parsedMetadata);
    jest.spyOn(blake, 'blake2bHex').mockReturnValueOnce(hash);

    const result = await service.validateMetadata(validateMetadataDTO);

    expect(result).toEqual({
      status: undefined,
      valid: true,
      metadata: parsedMetadata,
    });
    expect(validateMetadataStandard).toHaveBeenCalledWith(
      body.body,
      MetadataStandard.CIP108,
    );
    expect(parseMetadata).toHaveBeenCalledWith(body.body);
    expect(httpService.get).toHaveBeenCalledWith(
      url,
      expect.objectContaining({
        httpAgent: expect.any(Object),
        httpsAgent: expect.any(Object),
        maxRedirects: 0,
        proxy: false,
      }),
    );
  });

  it('should handle URL_NOT_FOUND error', async () => {
    const url = 'http://example.com';
    const hash = 'correctHash';
    const validateMetadataDTO: ValidateMetadataDTO = { hash, url };

    jest
      .spyOn(httpService, 'get')
      .mockReturnValueOnce(
        throwError(() => MetadataValidationStatus.URL_NOT_FOUND),
      );

    const result = await service.validateMetadata(validateMetadataDTO);

    expect(result).toEqual({
      status: MetadataValidationStatus.URL_NOT_FOUND,
      valid: false,
      metadata: undefined,
    });
  });

  it('should handle INVALID_HASH error', async () => {
    const url = 'http://example.com';
    const hash = 'incorrectHash';
    const validateMetadataDTO: ValidateMetadataDTO = { hash, url };
    const body = {
      body: 'testBody',
    };
    const parsedMetadata = { parsed: 'metadata' };

    const response: AxiosResponse = {
      data: JSON.stringify(body),
      status: 200,
      statusText: 'OK',
      headers: {},
      config: {
        headers: {} as AxiosRequestHeaders,
        url,
      },
    };
    jest.spyOn(httpService, 'get').mockReturnValueOnce(of(response));
    (getStandard as jest.Mock).mockReturnValueOnce(MetadataStandard.CIP108);
    (validateMetadataStandard as jest.Mock).mockResolvedValueOnce(undefined);
    (parseMetadata as jest.Mock).mockReturnValueOnce(parsedMetadata);
    jest.spyOn(blake, 'blake2bHex').mockReturnValueOnce('differentHash');

    const result = await service.validateMetadata(validateMetadataDTO);

    expect(result).toEqual({
      status: MetadataValidationStatus.INVALID_HASH,
      valid: false,
      metadata: parsedMetadata,
    });
  });

  it('should block loopback metadata URLs before fetching', async () => {
    const validateMetadataDTO: ValidateMetadataDTO = {
      hash: 'hash',
      url: 'http://127.0.0.1:3000/api',
    };

    const result = await service.validateMetadata(validateMetadataDTO);

    expect(result).toEqual({
      status: MetadataValidationStatus.URL_BLOCKED,
      valid: false,
      metadata: undefined,
    });
    expect(httpService.get).not.toHaveBeenCalled();
  });

  it.each([
    'http://[::]/metadata.json',
    'http://[::1]/metadata.json',
    'http://[fc00::1]/metadata.json',
    'http://[fe90::1]/metadata.json',
    'http://[febf::1]/metadata.json',
    'http://[ff02::1]/metadata.json',
    'http://[::ffff:7f00:1]/metadata.json',
  ])('should block special-use IPv6 URL %s before fetching', async (url) => {
    const result = await service.validateMetadata({ hash: 'hash', url });

    expect(result).toEqual({
      status: MetadataValidationStatus.URL_BLOCKED,
      valid: false,
      metadata: undefined,
    });
    expect(httpService.get).not.toHaveBeenCalled();
  });

  it.each([
    'http://192.0.2.1/metadata.json',
    'http://198.51.100.1/metadata.json',
    'http://203.0.113.1/metadata.json',
  ])('should block reserved IPv4 URL %s before fetching', async (url) => {
    const result = await service.validateMetadata({ hash: 'hash', url });

    expect(result).toEqual({
      status: MetadataValidationStatus.URL_BLOCKED,
      valid: false,
      metadata: undefined,
    });
    expect(httpService.get).not.toHaveBeenCalled();
  });

  it('should block hostnames that resolve to private addresses', async () => {
    (lookup as jest.Mock).mockResolvedValueOnce([{ address: '10.0.0.5' }]);

    const validateMetadataDTO: ValidateMetadataDTO = {
      hash: 'hash',
      url: 'https://metadata.internal.example/metadata.json',
    };

    const result = await service.validateMetadata(validateMetadataDTO);

    expect(result).toEqual({
      status: MetadataValidationStatus.URL_BLOCKED,
      valid: false,
      metadata: undefined,
    });
    expect(httpService.get).not.toHaveBeenCalled();
  });

  it('should block private addresses during the HTTP agent lookup', async () => {
    const url = 'http://example.com';
    const hash = 'correctHash';
    const body = {
      body: 'testBody',
    };
    const response: AxiosResponse = {
      data: JSON.stringify(body),
      status: 200,
      statusText: 'OK',
      headers: {},
      config: {
        headers: {} as AxiosRequestHeaders,
        url,
      },
    };
    jest.spyOn(httpService, 'get').mockReturnValueOnce(of(response));
    (getStandard as jest.Mock).mockReturnValueOnce(MetadataStandard.CIP108);
    (validateMetadataStandard as jest.Mock).mockResolvedValueOnce(undefined);
    jest.spyOn(blake, 'blake2bHex').mockReturnValueOnce(hash);

    await service.validateMetadata({ hash, url });

    const requestConfig = (httpService.get as jest.Mock).mock.calls[0][1];
    const agentLookup = requestConfig.httpAgent.options.lookup;
    (lookup as jest.Mock).mockResolvedValueOnce({
      address: '127.0.0.1',
      family: 4,
    });

    await expect(
      new Promise((resolve, reject) => {
        agentLookup('example.com', {}, (error: Error | null) => {
          if (error) {
            reject(error);
            return;
          }

          resolve(undefined);
        });
      }),
    ).rejects.toThrow(MetadataValidationStatus.URL_BLOCKED);
  });

  it('should preserve URL_BLOCKED from a connection-time lookup', async () => {
    const blockedError = Object.assign(
      new Error(MetadataValidationStatus.URL_BLOCKED),
      { code: MetadataValidationStatus.URL_BLOCKED },
    );
    jest
      .spyOn(httpService, 'get')
      .mockReturnValueOnce(throwError(() => blockedError));

    const result = await service.validateMetadata({
      hash: 'hash',
      url: 'http://example.com',
    });

    expect(result).toEqual({
      status: MetadataValidationStatus.URL_BLOCKED,
      valid: false,
      metadata: undefined,
    });
  });
});
