import { Controller, Get } from "@nestjs/common";
import { stat } from "fs";

@Controller('health')
export class HealthController {
    @Get()
    getHealth() {
        return {
            status: 'healthy',
        };
    }
}