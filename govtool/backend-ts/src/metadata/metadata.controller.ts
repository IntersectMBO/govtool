import { Body,Controller,Post, UsePipes, ValidationPipe } from "@nestjs/common";

import { ValidateMetadataDto } from "./dto/validate-metadata.dto";
import { MetadataService } from "./metadata.service";
import { ValidateMetadataResult } from "./metadata.type";

@Controller('metadata')
export class MetadataController {
    constructor(private readonly metadataService: MetadataService) {}

    @Post('validate')
    @UsePipes(
        new ValidationPipe({
            whitelist: true,
            forbidNonWhitelisted: true,
            transform: true,
        }),
    )
    validateMetadata(
        @Body() body: ValidateMetadataDto,
    ): Promise<ValidateMetadataResult> {
        return this.metadataService.validateMetadata(body);
    }
}