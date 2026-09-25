import { Request, Response } from 'express';
import { failureStatus } from '../helpers/Errors';
import * as service from '../services/metadataService';

const HASH_RE = /^[0-9a-fA-F]{64}$/;
const REPORT_ID_RE = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;

const parseHash = (value: unknown): Buffer | undefined =>
    typeof value === 'string' && HASH_RE.test(value) ? Buffer.from(value, 'hex') : undefined;

const isValidUrl = (value: unknown): value is string => {
    if (typeof value !== 'string' || !value) return false;
    try {
        new URL(value);
        return true;
    } catch {
        return false;
    }
};

/** GET /api/metadata?hash=&url= — the legacy resolve route. */
export const getMetadata = async (req: Request, res: Response): Promise<any> => {
    const hash = req.query.hash;
    const url = req.query.url as string | undefined;

    if (url && !isValidUrl(url)) {
        return res.status(400).json({ message: "Invalid url in request :", url });
    }
    const hashBuf = parseHash(hash);
    if (!hashBuf) {
        return res.status(400).json({ message: "Invalid hash provided in request", hash });
    }
    if (req.query.cip !== undefined) {
        return res.status(501).json({ message: "CIP validation is not available yet" });
    }

    const result = await service.resolve(hashBuf, url, req.headers['cache-control'] === 'invalidate');
    if (!result) {
        return res.status(400).json({ message: "Url not provided, Cached value not available" });
    }
    if (result.ok) {
        return res.status(200).json({
            hash: result.hash,
            fetchedAt: result.fetchedAt,
            url: result.url,
            metadata: result.body,
        });
    }
    return res.status(failureStatus(result.code, result.message)).json({
        code: result.code,
        category: result.category,
        message: result.message,
        url,
        fetchedAt: result.checkedAt,
        ...(result.servedHash ? { expectedHash: hashBuf.toString('hex'), servedHash: result.servedHash } : {}),
        ...(result.reportId ? { reportId: result.reportId } : {}),
    });
};

/** POST /api/metadata/:hash/refresh?url= — a retry, one real fetch per window. */
export const refreshMetadata = async (req: Request, res: Response): Promise<any> => {
    const hashBuf = parseHash(req.params.hash);
    const url = req.query.url;
    if (!hashBuf) return res.status(400).json({ message: "Invalid hash provided in request" });
    if (!isValidUrl(url)) return res.status(400).json({ message: "A valid url is required" });

    const outcome = await service.refresh(hashBuf, url);
    if (outcome.retryAfterSeconds) res.setHeader('Retry-After', String(outcome.retryAfterSeconds));
    return res.status(200).json(outcome);
};

/** GET /api/metadata/reports/:id */
export const getReport = async (req: Request, res: Response): Promise<any> => {
    const id = req.params.id;
    if (typeof id !== 'string' || !REPORT_ID_RE.test(id)) {
        return res.status(404).json({ message: "Report not found" });
    }
    const report = await service.getReport(id);
    if (!report) return res.status(404).json({ message: "Report not found" });
    return res.status(200).json(report);
};

/** GET /api/metadata/reports?hash=&url= — newest first. */
export const listReports = async (req: Request, res: Response): Promise<any> => {
    const hashBuf = parseHash(req.query.hash);
    const url = req.query.url;
    if (!hashBuf) return res.status(400).json({ message: "Invalid hash provided in request" });
    if (!isValidUrl(url)) return res.status(400).json({ message: "A valid url is required" });
    return res.status(200).json(await service.listReports(hashBuf, url));
};

process.on("uncaughtException", (err) => {
    console.error('Uncaught Exception:', err);
});

process.on('unhandledRejection', (reason, promise) => {
    console.error('Unhandled Rejection at:', promise, 'reason:', reason);
});
