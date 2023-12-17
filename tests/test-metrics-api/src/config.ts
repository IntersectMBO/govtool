import * as dotenv from 'dotenv'
import * as fs from 'fs'
import * as path from 'path'

dotenv.config()

interface Config {
  PGPASSWORD: string | undefined
  PGUSER: string | undefined
  API_SECRET_TOKEN: string | undefined
  PGPORT: string | undefined
  PGDATABASE: string | undefined
  PGHOST: string | undefined
}

const readSecretFromFile = (envVar: string): string | undefined => {
  if (process.env[envVar]) {
    return process.env[envVar]
  }
  const filePath = process.env[envVar + '_FILE'] || '/run/secrets/' + envVar.toLowerCase()

  try {
    return fs.readFileSync(filePath, 'utf8').trim()
  } catch (error) {
    console.error(`Error reading secret from file ${filePath}:`, error)
    process.exit(1)
  }
}
const readFromEnv = (envVar: string, defaultVal: string): string => {
  return process.env[envVar] || defaultVal
}

const config: Config = {
  PGPASSWORD: readSecretFromFile('PGPASSWORD'),
  PGUSER: readSecretFromFile('PGUSER'),
  API_SECRET_TOKEN: readSecretFromFile('API_SECRET_TOKEN'),
  PGPORT: readFromEnv('PGPORT', '5432'),
  PGDATABASE: readSecretFromFile('PGDATABASE'),
  PGHOST: readFromEnv('PGHOST', '/run/secrets/host'),
}

export default config
