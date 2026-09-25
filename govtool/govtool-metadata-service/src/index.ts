import './config/env';

process.env.ELASTIC_APM_SERVICE_NAME='drep-metadata-api'
process.env.ELASTIC_APM_ENVIRONMENT=process.env.ELASTIC_APM_ENVIRONMENT || 'local'
process.env.ELASTIC_APM_LOG_LEVEL='warning'

if(process.env.ELASTIC_APM_SERVER_URL && process.env.ELASTIC_APM_API_KEY){
  //@ts-ignore
  console.log("Enabling Elastic APM")
  require('elastic-apm-node').start();
}else{
  console.log("--- Elastic APM Disabled --- ")
}


import express from 'express';
import http from 'http';
import metadataRoutes from './routes/metadataRouter';
import dataBaseStoreRoute from './controllers/databaseStoreController'
import healthRouter from './controllers/health'

import {errorHandler} from './errors/AppError';
import {Response, Request} from 'express'
import path from 'path';
import setupSwaggerUi from './swagger-loader';
import fs from 'fs'
import {prisma} from './config/db';

const app = express();

const dynamicCors = (req: Request, res: Response, next: any) => {
    const origin = req.headers.origin

    // Allow requests from any origin but with credentials
    res.header('Access-Control-Allow-Origin', origin || '*');
    res.header('Access-Control-Allow-Methods', 'GET,HEAD,PUT,PATCH,POST,DELETE');
    res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
    res.header('Access-Control-Allow-Credentials', 'true');

    // Handle preflight requests
    if (req.method === 'OPTIONS') {
        res.sendStatus(200);
    } else {
        next();
    }
};
app.use(express.static(path.resolve('.')));

if (process.env.CORS_ENABLE) {
    app.use(dynamicCors);
}


// Middleware
app.use(express.json());
app.use(express.text())


// Order Routes
app.use('/api/metadata', metadataRoutes);
app.use('/api/data', dataBaseStoreRoute);
app.use('/api/health', healthRouter)


setupSwaggerUi(app)
const indexFile = path.resolve('.', './index.html')
// Check if index.html exists
fs.access(indexFile, fs.constants.F_OK, (err) => {
    if (!err) {
        // If index.html exists, define the catch-all handler
        app.get('/{*splat}', (req: Request, res: Response) => {
            res.sendFile(indexFile);
        });
    } else {
        console.error('index.html does not exist.');
    }
});
app.use(errorHandler);


// Create HTTP server
const server = http.createServer(app);

if (process.env.NODE_ENV !== 'test') {
    // Start the server
    const port = process.env.PORT || 3000;
    console.log("Connecting to database")
    prisma.$connect().then(() => {
        server.listen(port, () => {
            console.log(`Server is running on port ${port}`);
        });
    }).catch(e => {
        console.error("Database conn failed", e)
    })
}

export { app, server };
