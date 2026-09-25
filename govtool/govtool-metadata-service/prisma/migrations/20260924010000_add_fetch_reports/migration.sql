-- AlterTable
ALTER TABLE "metadata" ADD COLUMN "reportId" TEXT;

-- CreateTable
CREATE TABLE "fetch_report" (
    "id" TEXT NOT NULL,
    "hash" BYTEA NOT NULL,
    "url" TEXT NOT NULL,
    "code" TEXT NOT NULL,
    "message" TEXT NOT NULL,
    "servedHash" BYTEA,
    "bodyHash" BYTEA,
    "startedAt" TIMESTAMP(3) NOT NULL,
    "finishedAt" TIMESTAMP(3) NOT NULL,
    "details" JSONB NOT NULL,

    CONSTRAINT "fetch_report_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "fetch_body" (
    "hash" BYTEA NOT NULL,
    "data" BYTEA NOT NULL,

    CONSTRAINT "fetch_body_pkey" PRIMARY KEY ("hash")
);

-- CreateIndex
CREATE INDEX "fetch_report_url_hash_startedAt_idx" ON "fetch_report"("url", "hash", "startedAt");
