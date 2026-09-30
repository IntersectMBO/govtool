import {Router,Request, Response} from "express";
import {prisma} from "../config/db";

const router = Router();

router.get('/', async (req:Request, res:Response) => {
    try {
        // Perform a dummy query to test the connection (e.g., select 1)
        const result = await prisma.$queryRaw`SELECT 1 AS result`;

        if (result) {
            res.status(200).json({ isHealthy: true });
        } else {
            res.status(500).json({isHealthy:false, message: 'Database connection failed' });
        }
    } catch (error:any) {
        console.error('Error connecting to the database:', error);
        res.status(500).json({ message: 'Database connection failed', error: error.message });
    }
});

export default router