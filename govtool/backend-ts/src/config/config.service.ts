import { Injectable } from "@nestjs/common";
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
                host: process.env.VVA_DBSYNCCONFIG_HOST ?? rawConfig.dbsyncconfig.host,
                dbname: process.env.VVA_DBSYNCCONFIG_DBNAME ?? rawConfig.dbsyncconfig.dbname,
                user: process.env.VVA_DBSYNCCONFIG_USER ?? rawConfig.dbsyncconfig.user,
                password: process.env.VVA_DBSYNCCONFIG_PASSWORD ?? rawConfig.dbsyncconfig.password,
                port: Number(process.env.VVA_DBSYNCCONFIG_PORT ?? rawConfig.dbsyncconfig.port),
            },
            pinataApiJwt: process.env.VVA_PINATAAPIJWT ?? rawConfig.pinataapijwt ?? null,
            port: Number(process.env.VVA_PORT ?? rawConfig.port),
            host: String(process.env.VVA_HOST ?? rawConfig.host),
            cacheDurationSeconds: Number(
                process.env.VVA_CACHEDURATIONSECONDS ?? rawConfig.cachedurationseconds,
            ),
            drepListCacheDurationSeconds: Number(
                process.env.VVA_DREPLISTCACHEDURATIONSECONDS ??
                rawConfig.dreplistcachedurationseconds,
            ),
            sentryDsn: process.env.VVA_SENTRYDSN ?? rawConfig.sentrydsn,
            sentryEnv: process.env.VVA_SENTRYENV ?? rawConfig.sentryenv,
        };
    }

    private getConfigPath(): string {
        const args = process.argv;
        const confiFlagIndex = args.findIndex((arg) => arg === '-c' || arg === '--config');

        if(confiFlagIndex >= 0 && args[confiFlagIndex +1]) {
            return path.resolve(args[confiFlagIndex + 1]);
        }
       
        return path.resolve('example-config.json');
     }

     private readJsonConfig(configPath: string): BackendConfigFile {
        const file = fs.readFileSync(configPath, 'utf8');
        return JSON.parse(file) as BackendConfigFile;
     }
}