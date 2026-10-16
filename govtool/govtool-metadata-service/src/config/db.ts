import './env';
import apm from "elastic-apm-node";
import { PrismaPg } from '@prisma/adapter-pg';
import { PrismaClient } from '@prisma/client';
import { resolveSecret } from './secrets';

const adapter = new PrismaPg({ connectionString: resolveSecret('DATABASE_URL') });
const prismaClient = new PrismaClient({ adapter });
const apmEnabled = Boolean(
  process.env.ELASTIC_APM_SERVER_URL && process.env.ELASTIC_APM_API_KEY
);

export const prisma = apmEnabled
  ? prismaClient.$extends({
      query: {
        $allModels: {
          async $allOperations({ model, operation, args, query }) {
            const span = apm.startSpan(`prisma.${model}.${operation}`);
            if (span) {
              span.type = "DB";
              span.subtype = "prisma";
              span.action = "query";
            }

            try {
              return await query(args);
            } finally {
              span?.end();
            }
          },
        }
      },
    })
  : prismaClient;

export const disconnectPrisma = async () => {
    await prisma.$disconnect();
};
