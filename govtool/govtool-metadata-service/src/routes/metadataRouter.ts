import { handlerWrapper } from '../errors/AppError';
import { Router } from 'express';
import { getMetadata, getReport, listReports, refreshMetadata } from '../controllers/metadataController';

const router = Router();

router.get('/reports', handlerWrapper(listReports));
router.get('/reports/:id', handlerWrapper(getReport));
router.post('/:hash/refresh', handlerWrapper(refreshMetadata));
router.get('/', handlerWrapper(getMetadata));

export default router;
