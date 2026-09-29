import { Module } from '@nestjs/common';
import { CommentsController } from './comments.controller';
import { CommentsService } from './comments.service';

/** Comments and comment reports (§8.7, §8.12). */
@Module({
  controllers: [CommentsController],
  providers: [CommentsService],
})
export class CommentsModule {}
