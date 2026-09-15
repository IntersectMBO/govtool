import { Controller, Get, HttpException } from '@nestjs/common';

@Controller()
export class SystemController {
  @Get('throw500')
  throw500(): never {
    throw new HttpException(
      {
        errorType: 'CriticalError',
        message: 'intentional system break for testing purposes',
      },
      500,
    );
  }
}
