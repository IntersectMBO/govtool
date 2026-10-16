import { Controller, Get, Put } from '@nestjs/common';
import type { AuthUser } from '../auth/auth-user';
import { Authenticated, Caller } from '../auth/auth.guard';
import { RawBody } from '../common/body';
import { badRequest, isUniqueViolation } from '../common/errors';
import { PrismaService } from '../prisma/prisma.service';
import { isValidUsername, selfProjection } from './self-projection';

/** §7.7. Raw self projection responses. */
@Controller('users')
@Authenticated()
export class UsersController {
  constructor(private readonly prisma: PrismaService) {}

  @Get('me')
  me(@Caller() caller: AuthUser) {
    return selfProjection(caller.row);
  }

  @Put('edit')
  async edit(@Caller() caller: AuthUser, @RawBody() body: Record<string, unknown>) {
    if (!Object.prototype.hasOwnProperty.call(body, 'govtoolUsername') || body.govtoolUsername == null) {
      throw badRequest('Missing parameters for user update.');
    }
    const name = body.govtoolUsername;
    if (!isValidUsername(name)) {
      throw badRequest(
        'Failed to update user: govtool_username must match the following: "^(?![._])[a-z0-9._]{1,30}$"',
      );
    }
    try {
      const user = await this.prisma.user.update({
        where: { id: caller.id },
        data: { govtoolUsername: name },
      });
      return selfProjection(user);
    } catch (e) {
      if (isUniqueViolation(e)) throw badRequest('Failed to update user: This attribute must be unique');
      throw e;
    }
  }
}
