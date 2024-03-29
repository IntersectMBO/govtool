import { Controller, Get, Post } from '@nestjs/common';
import { AppService } from './app.service';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    return this.appService.getHello();
  }

  @Post('create-metadata')
  createMetadata(): null {
    return this.appService.createMetadata();
  }

  @Post('validate-metadata')
  validateMetadata(): null {
    return this.appService.validateMetadata();
  }
}
