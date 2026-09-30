/*
  Warnings:

  - The primary key for the `metadata` table will be changed. If it partially fails, the table could be left without primary key constraint.
  - Made the column `url` on table `metadata` required. This step will fail if there are existing NULL values in that column.

*/
-- AlterTable
ALTER TABLE "metadata" DROP CONSTRAINT "metadata_pkey",
ADD COLUMN     "id" SERIAL NOT NULL,
ALTER COLUMN "hash" DROP NOT NULL,
ALTER COLUMN "data" DROP NOT NULL,
ALTER COLUMN "url" SET NOT NULL,
ADD CONSTRAINT "metadata_pkey" PRIMARY KEY ("id");

-- CreateIndex
CREATE INDEX "metadata_hash_idx" ON "metadata"("hash");

-- CreateIndex
CREATE INDEX "metadata_url_idx" ON "metadata"("url");
