-- CreateTable
CREATE TABLE "datastore" (
    "name" TEXT NOT NULL,
    "value" BYTEA NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "datastore_pkey" PRIMARY KEY ("name")
);
