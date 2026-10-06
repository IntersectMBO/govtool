import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { configureApp } from './app.setup';
import { AppConfig, ConfigError, loadConfig } from './config/config';
import { loadSecretsIntoEnv } from './config/secrets';

async function bootstrap() {
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
void bootstrap();
