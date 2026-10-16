import { Controller, Get } from '@nestjs/common';
import { Public } from '../auth/auth.guard';
import { RawHttpError } from '../common/errors';
import { PrismaService } from '../prisma/prisma.service';

/** `GET /health`, outside `/api`, not enveloped (§2). */
@Controller('health')
@Public()
export class HealthController {
  constructor(private readonly prisma: PrismaService) {}

  @Get()
  async health() {
    try {
      await this.prisma.$queryRaw`SELECT 1`;
    } catch {
      throw new RawHttpError(503, { status: 'error' });
    }
    return { status: 'ok' };
  }
}
