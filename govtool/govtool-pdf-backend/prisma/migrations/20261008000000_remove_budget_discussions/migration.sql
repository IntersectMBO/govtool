-- Budget discussions are removed (D167): the 2025 budget proposals are a
-- static archive in the frontend, so the forum keeps proposals only.
-- Applies to databases that hold BD rows; nothing here is exported first.

-- Comments become proposal-only. Replies and reports of a BD comment go
-- with it (parent_id and comments_reports.comment_id cascade).
DELETE FROM "comments" WHERE "bd_master_id" IS NOT NULL;

ALTER TABLE "comments" DROP CONSTRAINT "comments_target_check";
ALTER TABLE "comments" DROP CONSTRAINT "comments_bd_master_id_fkey";
DROP INDEX "comments_bd_master_id_parent_id_created_at_idx";
ALTER TABLE "comments" DROP COLUMN "bd_master_id";
ALTER TABLE "comments" ALTER COLUMN "proposal_id" SET NOT NULL;

-- BD tables, referencing tables first. RESTRICT, so a dependency missed
-- here fails the migration instead of being dropped with it.
DROP TABLE "bd_poll_votes" RESTRICT;
DROP TABLE "bd_polls" RESTRICT;
DROP TABLE "bd_drafts" RESTRICT;
DROP TABLE "bd_links" RESTRICT;
DROP TABLE "bds" RESTRICT;
DROP TABLE "bd_costings" RESTRICT;
DROP TABLE "bd_proposal_details" RESTRICT;
DROP TABLE "bd_psapbs" RESTRICT;
DROP TABLE "bd_proposal_ownerships" RESTRICT;
DROP TABLE "bd_further_informations" RESTRICT;
DROP TABLE "bd_contact_informations" RESTRICT;

-- Lookups only BDs used.
DROP TABLE "bd_types" RESTRICT;
DROP TABLE "bd_road_maps" RESTRICT;
DROP TABLE "bd_intersect_committees" RESTRICT;
DROP TABLE "bd_contract_types" RESTRICT;
DROP TABLE "bd_currency_lists" RESTRICT;
DROP TABLE "country_lists" RESTRICT;
