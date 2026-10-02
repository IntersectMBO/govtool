import { handlerWrapper } from '../errors/AppError';
import { Router } from 'express';
import { getMetadata, getReport, listReports, refreshMetadata, verifyMetadata } from '../controllers/metadataController';

const router = Router();

router.get('/reports', handlerWrapper(listReports));
router.get('/reports/:id', handlerWrapper(getReport));
router.post('/:hash/refresh', handlerWrapper(refreshMetadata));
router.post('/:hash/verify', handlerWrapper(verifyMetadata));
router.get('/', handlerWrapper(getMetadata));

export default router;
