-- CreateTable
CREATE TABLE "metadata" (
    "hash" BYTEA NOT NULL,
    "data" BYTEA NOT NULL,
    "url" TEXT,
    "fetchedAt" TIMESTAMP(3) NOT NULL,
    "error" TEXT,

    CONSTRAINT "metadata_pkey" PRIMARY KEY ("hash")
);
