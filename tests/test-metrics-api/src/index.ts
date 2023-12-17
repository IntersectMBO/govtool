import express, { Express, NextFunction, Request, Response } from 'express'
import config from './config'
import { endpointMetricsToDb, migrate, pool, testMetricsToDb } from './db'

const app: Express = express()
const port = process.env.server_PORT || 8080
app.use(express.json({ limit: '200mb' }))
app.use(express.urlencoded({ extended: true }))
migrate()

const checkSecretToken = (req: Request, res: Response, next: NextFunction) => {
  const key = req.get('secret-token')

  if (key !== config.API_SECRET_TOKEN) {
    res.status(403).json({ message: 'Secret Token missing or invalid' })
    return
  }
  next() // Call next to proceed to the next middleware or route handler
}

app.use(checkSecretToken)


async function uploadMetrics(req: Request, res: Response, uploadFunction: (file: any) => Promise<void>) {
  try {
    const file = req.body
    if (!file) {
      throw 'File not uploaded'
    }
    await uploadFunction(file)
    res.send('Metrics have been successfully uploaded to database')
  } catch (e) {
    console.log('Error: ', e)
    res.status(400).send('Metrics upload not successful')
  }
}

app.post('/metrics/test-results', async (req: Request, res: Response) => {
  uploadMetrics(req, res, testMetricsToDb)
})

app.post('/metrics/api-endpoints', async (req: Request, res: Response) => {
  uploadMetrics(req, res, endpointMetricsToDb)
})
pool.connect().then(() =>
  app.listen(port, () => {
    console.log(`⚡️[server]: Server is running at http://localhost:${port}`)
  })
)
