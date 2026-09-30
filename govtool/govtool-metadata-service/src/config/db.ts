import './env';
import apm from "elastic-apm-node";
import { PrismaPg } from '@prisma/adapter-pg';
import { PrismaClient } from '@prisma/client';

const adapter = new PrismaPg({ connectionString: process.env.DATABASE_URL });
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

process.on('SIGTERM', async () => {
  console.log('SIGTERM signal received: closing HTTP server');

  console.log('Disconnecting Prisma Client');
  await prisma.$disconnect();
});

export const disconnectPrisma = async () => {
    await prisma.$disconnect();
};
