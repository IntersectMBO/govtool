import { Injectable } from '@nestjs/common';

@Injectable()
export class AppService {
  getHello(): string {
    return 'Hello World!';
  }

  createMetadata(): null {
    return null;
  }

  validateMetadata(): null {
    return null;
  }
}
