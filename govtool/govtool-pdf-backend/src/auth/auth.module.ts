import { Global, Module } from '@nestjs/common';
import { APP_GUARD } from '@nestjs/core';
import { UsersController } from '../users/users.controller';
import { AuthController } from './auth.controller';
import { AuthGuard } from './auth.guard';
import { AuthService } from './auth.service';
import { TokensService } from './tokens.service';

/**
 * Login, tokens, the users routes and the global AuthGuard. Global so any
 * module can inject TokensService.
 */
@Global()
@Module({
  controllers: [AuthController, UsersController],
  providers: [AuthService, TokensService, { provide: APP_GUARD, useClass: AuthGuard }],
  exports: [TokensService],
})
export class AuthModule {}
