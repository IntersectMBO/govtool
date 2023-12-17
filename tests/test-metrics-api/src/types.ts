// Define TypeScript types for the various JSON structures
export interface TestMetricsJson {
  outcome: string
  start_date: number
  end_date: number
  build_id: string
  test_name: string
  commit_hash: string
}

export interface EndpointMetricsJson {
  build_id: string
  method: string
  endpoint: string
  path_param?: string
  json?: any // Replace 'any' with a more specific type if possible
  status_code: number
  response_json?: any // Replace 'any' with a more specific type if possible
  response_time: number
  start_date: number
}

export interface LighthouseAuditMetric {
  numericValue: number
  numericUnit: 'ms' | 's'
}

// Complete Lighthouse Metrics JSON Type
export interface LighthouseMetricsJson {
  audits: {
    interactive: LighthouseAuditMetric
    'first-contentful-paint': LighthouseAuditMetric
    'speed-index': LighthouseAuditMetric
    'total-blocking-time': LighthouseAuditMetric
    'largest-contentful-paint': LighthouseAuditMetric
    'cumulative-layout-shift': LighthouseAuditMetric
    [auditName: string]: LighthouseAuditMetric // In case there are more metrics that follow the same pattern
  }
  fetchTime: string
}
