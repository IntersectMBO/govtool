import { Injectable } from "@nestjs/common";
import 'dotenv/config'
import * as fs from 'fs';
import * as path from 'path';

import { BackendConfig, BackendConfigFile } from "./config.types";

@Injectable()
export class ConfigService {
    private readonly config: BackendConfig;

    constructor() {
        this.config = this.loadConfig();
    }

    get(): BackendConfig {
        return this.config;
    }

    getDbConnectionConfig(){
        return {
            host: this.config.dbSync.host,
            database: this.config.dbSync.dbname,
            user: this.config.dbSync.user,
            password: this.config.dbSync.password,
            port: this.config.dbSync.port,
        };
    }

    private loadConfig(): BackendConfig {
        const configPath = this.getConfigPath();
        const rawConfig = this.readJsonConfig(configPath);

        return {
            dbSync: {
            host: this.requiredEnvString('VVA_DBSYNCCONFIG_HOST'),
            dbname: this.requiredEnvString('VVA_DBSYNCCONFIG_DBNAME'),
            user: this.requiredEnvString('VVA_DBSYNCCONFIG_USER'),
            password: this.requiredEnvString('VVA_DBSYNCCONFIG_PASSWORD'),
            port: this.envNumber('VVA_DBSYNCCONFIG_PORT', 5432),
        },
        pinataApiJwt:
            this.envString('VVA_PINATAAPIJWT', rawConfig.pinataapijwt ?? '') || null,
        port: this.envNumber('VVA_PORT', rawConfig.port),
        host: this.envString('VVA_HOST', rawConfig.host),
        cacheDurationSeconds: this.envNumber(
            'VVA_CACHEDURATIONSECONDS',
            rawConfig.cachedurationseconds,
        ),
        drepListCacheDurationSeconds: this.envNumber(
            'VVA_DREPLISTCACHEDURATIONSECONDS',
            rawConfig.dreplistcachedurationseconds,
        ),
        sentryDsn: this.envString('VVA_SENTRYDSN', rawConfig.sentrydsn),
        sentryEnv: this.envString('VVA_SENTRYENV', rawConfig.sentryenv),
    };
    }

    private getConfigPath(): string {
        const args = process.argv;
        const confiFlagIndex = args.findIndex((arg) => arg === '-c' || arg === '--config');

        if(confiFlagIndex >= 0 && args[confiFlagIndex +1]) {
            return path.resolve(args[confiFlagIndex + 1]);
        }
       
        return path.resolve('config.json');
     }

     private readJsonConfig(configPath: string): BackendConfigFile {
        const file = fs.readFileSync(configPath, 'utf8');
        return JSON.parse(file) as BackendConfigFile;
     }
    
    private envNumber(name: string, fallback: number): number {
    const value = process.env[name];

    if (value === undefined || value.trim() === '') {
      return fallback;
    }

    const parsed = Number(value);

    if (Number.isNaN(parsed)) {
      throw new Error(`${name} must be a valid number`);
    }

    return parsed;
    }

    private requiredEnvString(name: string): string {
    const value = process.env[name];

    if (value === undefined || value.trim() === '') {
        throw new Error(`${name} is required`);
    }
     return value;
    }

     private envString(name: string, fallback: string): string {
    const value = process.env[name];

    if (value === undefined || value.trim() === '') {
      return fallback;
    }

    return value;
  }
}