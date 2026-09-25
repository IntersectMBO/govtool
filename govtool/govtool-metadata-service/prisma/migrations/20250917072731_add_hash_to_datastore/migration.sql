/*
  Warnings:

  - A unique constraint covering the columns `[hash]` on the table `datastore` will be added. If there are existing duplicate values, this will fail.

*/
-- AlterTable
ALTER TABLE "datastore" ADD COLUMN     "hash" TEXT;

-- CreateIndex
CREATE UNIQUE INDEX "datastore_hash_key" ON "datastore"("hash");

-- CreateIndex
CREATE INDEX "datastore_hash_idx" ON "datastore"("hash");
