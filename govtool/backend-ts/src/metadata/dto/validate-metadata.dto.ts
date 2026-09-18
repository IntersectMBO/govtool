import { IsEnum, IsNotEmpty, IsOptional, IsString } from "class-validator";

import { MetadataStandard } from "../metadata.type";

export class ValidateMetadataDto {
    @IsString()
    @IsNotEmpty()
    url!: string;

    @IsString()
    @IsNotEmpty()
    hash!: string;

    @IsOptional()
    @IsEnum(MetadataStandard)
    standard?: MetadataStandard;
}