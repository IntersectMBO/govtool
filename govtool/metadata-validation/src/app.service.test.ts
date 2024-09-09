import { Test, TestingModule } from '@nestjs/testing';
import { HttpService } from '@nestjs/axios';
import { of, throwError } from 'rxjs';
import * as blake from 'blakejs';

import { AppService } from './app.service';
import { ValidateMetadataDTO } from '@dto';
import { MetadataValidationStatus } from '@enums';
import { MetadataStandard } from '@types';
import { validateMetadataStandard, parseMetadata } from '@utils';
import { AxiosResponse, AxiosRequestHeaders } from 'axios';

jest.mock('@utils');

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
  });

  it('should validate metadata correctly', async () => {
    const url = 'http://example.com';
    const hash = 'correctHash';
    const validateMetadataDTO: ValidateMetadataDTO = { hash, url };
    const data = {
      body: 'testBody',
      headers: {},
    };
    const parsedMetadata = { parsed: 'metadata' };
    const response: AxiosResponse = {
      data,
      status: 200,
      statusText: 'OK',
      headers: {},
      config: {
        headers: {} as AxiosRequestHeaders,
        url,
      },
    };
    jest.spyOn(httpService, 'get').mockReturnValueOnce(of(response));
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
      data,
      MetadataStandard.CIP108,
    );
    expect(parseMetadata).toHaveBeenCalledWith(
      data.body,
      MetadataStandard.CIP108,
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
    const data = {
      body: 'testBody',
    };
    const parsedMetadata = { parsed: 'metadata' };

    const response: AxiosResponse = {
      data,
      status: 200,
      statusText: 'OK',
      headers: {},
      config: {
        headers: {} as AxiosRequestHeaders,
        url,
      },
    };
    jest.spyOn(httpService, 'get').mockReturnValueOnce(of(response));
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
});
