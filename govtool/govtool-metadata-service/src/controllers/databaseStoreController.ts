import { Router, Request, Response } from 'express';
import { prisma } from '../config/db';
import { blake2b } from 'libcardano'; // Import blake2b for hash calculation

const router = Router();
type IdentifierParams = { identifier: string };

// Set the maximum allowed file size to 4 MB (4 * 1024 * 1024 bytes)
const MAX_FILE_SIZE = 4 * 1024 * 1024;  // 4 MB

// Helper to determine if a string is a valid blake2b-256 hash
const isHash = (str: string): boolean => {
    return /^[0-9a-fA-F]{64}$/.test(str);
};

// PUT endpoint to save data by filename or hash
router.put('/:identifier', async (req: Request<IdentifierParams>, res: Response) => {
    const { identifier } = req.params;
    const data = req.body;
    let fileContentBuffer: Buffer;

    console.log('PUT request received for identifier:', identifier);
    console.log('req.body:', req.body);
    console.log('req.headers[\'content-type\']:', req.headers['content-type']);

    // Determine if the body was parsed by express.json() or express.text()
    if (typeof req.body === 'object' && Object.keys(req.body).length > 0) {
        console.log('Body parsed as JSON object.');
        fileContentBuffer = Buffer.from(JSON.stringify(req.body));
    } else if (typeof req.body === 'string') {
        console.log('Body parsed as plain text string.');
        fileContentBuffer = Buffer.from(req.body);
    } else {
        console.log('Body not parsed, reading from stream.');
        const chunks: Buffer[] = [];
        req.on('data', (chunk) => {
            chunks.push(chunk);
        });
        fileContentBuffer = await new Promise<Buffer>((resolve, reject) => {
            req.on('end', () => {
                resolve(Buffer.concat(chunks));
            });
            req.on('error', (err) => {
                reject(err);
            });
        });
    }
    console.log('fileContentBuffer length:', fileContentBuffer.length);
    console.log('fileContentBuffer content (first 50 chars):', fileContentBuffer.toString('utf-8').substring(0, 50));


    // Check if the file content exceeds the 4 MB limit
    if (fileContentBuffer.length > MAX_FILE_SIZE) {
        console.log('Error: File size exceeds limit.');
        return res.status(400).json({ message: 'File size exceeds 4 MB limit' });
    }

    let hash: string | undefined;
    let filename: string | undefined;
    const calculatedHash = blake2b.hash32(fileContentBuffer).toString('hex'); // Calculate hash from raw buffer
    console.log('Calculated hash:', calculatedHash);

    if (isHash(identifier)) {
        hash = identifier;
        filename = identifier; 
        if (calculatedHash !== hash) {
            console.log('Error: Hash mismatch in PUT for hash identifier.');
            return res.status(400).json({ message: 'Hash mismatch', expectedHash: hash, actualHash: calculatedHash });
        }
    } else {
        filename = identifier;
        hash = calculatedHash; // Store the calculated hash for filename-based uploads
        console.log('Identifier is filename, storing calculated hash:', hash);
    }

    try {
        const value = fileContentBuffer as Uint8Array<ArrayBuffer>;
        await prisma.datastore.upsert({
            where: { name: filename },
            update: { value, hash, updatedAt: new Date() },
            create: { name: filename, value, hash, createdAt: new Date(), updatedAt: new Date() },
        });
        console.log('Data stored successfully in DB.');
        res.status(200).json({ message: 'Data stored successfully', identifier: filename, hash: hash });
    } catch (error: any) {
        console.error('Error storing data:', error);
        res.status(500).json({ message: 'Failed to save data' });
    }
});

// GET endpoint to retrieve data by filename or hash
router.get('/:identifier', async (req: Request<IdentifierParams>, res: Response) => {
    const { identifier } = req.params;
    console.log('GET request received for identifier:', identifier);

    try {
        let storedData;
        if (isHash(identifier)) {
            console.log('Identifier is a hash, searching by hash.');
            storedData = await prisma.datastore.findUnique({
                where: { hash: identifier },
            });
        } else {
            console.log('Identifier is a filename, searching by name.');
            storedData = await prisma.datastore.findUnique({
                where: { name: identifier },
            });
        }

        if (!storedData) {
            console.log('Data not found for identifier:', identifier);
            return res.status(404).json({ message: 'Data not found' });
        }
        console.log('Data found in DB. Value length:', storedData.value.length);
        const value = Buffer.from(storedData.value);
        console.log('Data found in DB. Value content (first 50 chars):', value.toString('utf-8').substring(0, 50));


        // Send the content as a string. Content-Type will be inferred by Express.
        res.status(200).send(value.toString('utf-8'));
        console.log('Response sent with status 200 and raw content.');
    } catch (error: any) {
        console.error('Error retrieving data:', error);
        res.status(500).json({ message: 'Failed to retrieve data' });
    }
});

// DELETE endpoint to delete data by filename or hash
router.delete('/:identifier', async (req: Request<IdentifierParams>, res: Response) => {
    const { identifier } = req.params;

    try {
        let deleteResult;
        if (isHash(identifier)) {
            deleteResult = await prisma.datastore.delete({
                where: { hash: identifier },
            });
        } else {
            deleteResult = await prisma.datastore.delete({
                where: { name: identifier },
            });
        }

        res.status(200).json({ message: 'Data deleted successfully', identifier: identifier });
    } catch (error: any) {
        console.error(error);

        // Handle case where data does not exist
        if (error.code === 'P2025') {
            return res.status(404).json({ message: 'Data not found' });
        }

        res.status(500).json({ message: 'Failed to delete data' });
    }
});

export default router;
