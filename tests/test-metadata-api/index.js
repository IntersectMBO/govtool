const express = require('express');
const fs = require('fs');
const path = require('path');
const lock_api = require('./locks_api')
const ipfs_api = require('./ipfs_api')

const swaggerUi = require('swagger-ui-express');
const swaggerJsdoc = require('swagger-jsdoc');
const app = express();



const dynamicCors = (req, res, next) => {
    const origin = req.headers.origin

    // Allow requests from any origin but with credentials
    res.header('Access-Control-Allow-Origin', origin || '*');
    res.header('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE,OPTIONS');
    res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
    res.header('Access-Control-Allow-Credentials', 'true');

    // Handle preflight requests
    if (req.method === 'OPTIONS') {
        res.sendStatus(200);
    } else {
        next();
    }
};

const dataDir = process.env.DATA_DIR || path.join(__dirname, 'json_files');

if (!fs.existsSync(dataDir)) {
    fs.mkdirSync(dataDir, { recursive: true });
}
// cors enable
app.use(dynamicCors);

// IPFS pinning and gateway routes; they parse their own raw bodies, so they
// are set up before the text parser below.
ipfs_api.setup(app, { ipfsDir: process.env.IPFS_DIR || path.join(dataDir, 'ipfs') });

// Middleware to parse text request bodies
app.use(express.text());

// Swagger configuration
const swaggerOptions = {
    definition: {
        openapi: '3.0.0',
        info: {
            title: 'File API',
            version: '1.0.0',
            description: 'API for saving and deleting files',
        },
    },
    apis: ['index.js','locks_api.js','ipfs_api.js'], // Update the path to reflect the compiled JavaScript file
};

const swaggerSpec = swaggerJsdoc(swaggerOptions);

// Serve Swagger UI
app.use('/docs', swaggerUi.serve, swaggerUi.setup(swaggerSpec));

// PUT endpoint to save a file
// Filenames are single path segments; anything that could leave dataDir
// (separators, "..", NUL) or reach the ipfs store is refused.
function resolveDataPath(filename) {
    if (!filename || filename === '.' || filename === '..' || filename === 'ipfs'
        || /[\\/\0]/.test(filename)) {
        return null;
    }
    return path.join(dataDir, filename);
}

/**
 * @swagger
 * /data/{filename}:
 *   put:
 *     summary: Save data to a file
 *     tags: [Metadata File]
 *     parameters:
 *       - in: path
 *         name: filename
 *         schema:
 *           type: string
 *         required: true
 *         description: The name of the file to save
 *     requestBody:
 *       required: true
 *       content:
 *         text/plain:
 *           schema:
 *             type: string
 *     responses:
 *       '201':
 *         description: File saved successfully
 */
app.put('/data/:filename', (req, res) => {
    const filePath = resolveDataPath(req.params.filename);
    if (!filePath) {
        return res.status(400).send({'message': 'Invalid filename'});
    }

    fs.writeFile(filePath, req.body, (err) => {
        if (err) {
            console.error(err);
            return res.status(500).send('Failed to save file');
        }
        res.status(201).send({'success': true});
    });
});


// GET endpoint to retrieve a file
/**
 * @swagger
 * /data/{filename}:
 *   get:
 *     summary: Get a file
 *     tags: [Metadata File]
 *     parameters:
 *       - in: path
 *         name: filename
 *         schema:
 *           type: string
 *         required: true
 *         description: The name of the file to retrieve
 *     responses:
 *       '200':
 *         description: File retrieved successfully
 *         content:
 *           text/plain:
 *             schema:
 *               type: string
 */
app.get('/data/:filename', (req, res) => {
    const filePath = resolveDataPath(req.params.filename);
    if (!filePath) {
        return res.status(400).send({'message': 'Invalid filename'});
    }

    fs.readFile(filePath, 'utf8', (err, data) => {
        if (err) {
            console.error(err);
            return res.status(404).send({'message': 'File not found'});
        }
        res.status(200).send(data);
    });
});



// DELETE endpoint to delete a file
/**
 * @swagger
 * /data/{filename}:
 *   delete:
 *     summary: Delete a file
 *     tags: [Metadata File]
 *     parameters:
 *       - in: path
 *         name: filename
 *         schema:
 *           type: string
 *         required: true
 *         description: The name of the file to delete
 *     responses:
 *       '200':
 *         description: File deleted successfully
 */
app.delete('/data/:filename', (req, res) => {
    const filePath = resolveDataPath(req.params.filename);
    if (!filePath) {
        return res.status(400).send({'message': 'Invalid filename'});
    }

    fs.unlink(filePath, (err) => {
        if (err) {
            console.error(err);
            return res.status(500).send({'message':'Failed to delete file'});
        }
        res.send('File deleted successfully');
    });
});

app.get('/', (req, res) => {
    res.redirect('/docs');
});
lock_api.setup(app)
// Start the server
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
    console.log(`Server is running on port ${PORT}`);
});
