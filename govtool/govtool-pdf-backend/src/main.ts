import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { configureApp } from './app.setup';
import { AppConfig, ConfigError, loadConfig } from './config/config';
import { loadSecretsIntoEnv } from './config/secrets';

export async function bootstrap(): Promise<void> {
  loadSecretsIntoEnv();
  let config: AppConfig;
  try {
    config = loadConfig();
  } catch (e) {
    if (e instanceof ConfigError) {
      console.error(e.message);
      process.exit(1);
    }
    throw e;
  }
  const app = await NestFactory.create(AppModule, { bodyParser: false });
  configureApp(app, config);
  await app.listen(config.port, config.host);
}

// Run directly (`npm run start`); dist/start.js imports it instead.
if (require.main === module) {
  void bootstrap();
}
