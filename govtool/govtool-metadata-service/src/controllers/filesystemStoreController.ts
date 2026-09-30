import { Router, Request, Response } from 'express';
import fs from 'fs';
import path from 'path';

const router = Router();
const dataDir = process.env.DATA_DIR || path.join(__dirname, 'json_files');
type FilenameParams = { filename: string };

// Ensure the directory exists
if (!fs.existsSync(dataDir)) {
    fs.mkdirSync(dataDir, { recursive: true });
}

// PUT endpoint to save a file
router.put('/:filename', (req: Request<FilenameParams>, res: Response) => {
    const { filename } = req.params;
    const filePath = path.join(dataDir, filename);

    fs.writeFile(filePath, req.body, (err) => {
        if (err) {
            console.error(err);
            return res.status(500).json({ message: 'Failed to save file' });
        }
        res.status(201).json({ success: true });
    });
});

// GET endpoint to retrieve a file
router.get('/:filename', (req: Request<FilenameParams>, res: Response) => {
    const { filename } = req.params;
    const filePath = path.join(dataDir, filename);

    fs.readFile(filePath, 'utf8', (err, data) => {
        if (err) {
            console.error(err);
            return res.status(404).json({ message: 'File not found' });
        }
        res.status(200).send(data);
    });
});

// DELETE endpoint to delete a file
router.delete('/:filename', (req: Request<FilenameParams>, res: Response) => {
    const { filename } = req.params;
    const filePath = path.join(dataDir, filename);

    fs.unlink(filePath, (err) => {
        if (err) {
            console.error(err);
            return res.status(500).json({ message: 'Failed to delete file' });
        }
        res.status(200).send('File deleted successfully');
    });
});

export default router;
