const express = require('express');
const fs = require('fs');
const path = require('path');
const lock_api = require('./locks_api')

const swaggerUi = require('swagger-ui-express');
const swaggerJsdoc = require('swagger-jsdoc');
const app = express();


const dataDir = process.env.DATA_DIR || path.join(__dirname, 'json_files');

if (!fs.existsSync(dataDir)) {
    fs.mkdirSync(dataDir, { recursive: true });
}
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
    apis: ['index.js','locks_api.js'], // Update the path to reflect the compiled JavaScript file
};

const swaggerSpec = swaggerJsdoc(swaggerOptions);

// Serve Swagger UI
app.use('/docs', swaggerUi.serve, swaggerUi.setup(swaggerSpec));

// PUT endpoint to save a file
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
    const filename = req.params.filename;
    const filePath = path.join(dataDir, filename);

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
    const filename = req.params.filename;
    const filePath = path.join(dataDir, filename);

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
    const filename = req.params.filename;
    const filePath = path.join(dataDir, filename);

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
