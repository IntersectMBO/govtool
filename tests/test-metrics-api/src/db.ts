import pg from 'pg'
import config from './config'
import { EndpointMetricsJson, LighthouseMetricsJson, TestMetricsJson } from './types'

export const pool = new pg.Pool({
  host: config.PGHOST,
  port: parseInt(config.PGPORT as string),
  database: config.PGDATABASE,
  user: config.PGUSER,
  password: config.PGPASSWORD,
})

export async function migrate() {
  let createTableQuery = `
    CREATE TABLE IF NOT EXISTS test_metrics (
        id SERIAL PRIMARY KEY,
        outcome TEXT NOT NULL,
        start_date BIGINT NOT NULL,
        end_date BIGINT NOT NULL,
        build_id TEXT NOT NULL,
        test_name TEXT NOT NULL,
        commit_hash TEXT NOT NULL

  )`
  await pool.query(createTableQuery)
  createTableQuery = `
    CREATE TABLE IF NOT EXISTS endpoint_metrics (
        id SERIAL PRIMARY KEY,
        build_id TEXT NOT NULL,
        method TEXT NOT NULL,
        endpoint TEXT NOT NULL,
        path_param TEXT,
        json TEXT,
        status_code INTEGER NOT NULL,
        response_json TEXT ,
        response_time BIGINT NOT NULL,
        start_date BIGINT NOT NULL
  )`
  await pool.query(createTableQuery)
}

export async function testMetricsToDb(json: TestMetricsJson) {
  const insertQuery = `
      INSERT INTO test_metrics (outcome, start_date, end_date, build_id, test_name, commit_hash)
      VALUES ($1, $2, $3, $4, $5, $6)`
  await pool.query(insertQuery, [json.outcome, json.start_date, json.end_date, json.build_id, json.test_name, json.commit_hash])
}

export async function endpointMetricsToDb(json: EndpointMetricsJson) {
  const insertQuery = `
      INSERT INTO endpoint_metrics (build_id, method, endpoint, path_param, json, status_code, response_json, response_time, start_date)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)`
  await pool.query(insertQuery, [
    json.build_id,
    json.method,
    json.endpoint,
    json.path_param,
    json.json,
    json.status_code,
    json.response_json,
    json.response_time,
    json.start_date,
  ])
}
