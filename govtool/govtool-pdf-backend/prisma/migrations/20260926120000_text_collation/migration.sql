-- Linguistic, case-insensitive ordering for the text columns that list
-- routes sort on (SPEC §4.4), as Strapi had on a glibc Postgres. The alpine
-- image's libc collates "en_US.utf8" byte-wise, so "SORT b" < "Sort a" <
-- "sort c" there; ICU's root collation ("und-x-icu") orders a < b < c
-- whatever the case. It is deterministic, so unique indexes keep their
-- meaning, and ILIKE (`$containsi`) works on it.
--
-- Prisma cannot declare a column collation and `prisma migrate diff` does not
-- compare it, so later generated migrations leave these columns alone.
-- Types and lengths are unchanged; only the collation is set.

-- Proposals and comments
ALTER TABLE "proposal_contents" ALTER COLUMN "name" TYPE VARCHAR(80) COLLATE "und-x-icu";
ALTER TABLE "proposal_contents" ALTER COLUMN "abstract" TYPE TEXT COLLATE "und-x-icu";
ALTER TABLE "proposal_contents" ALTER COLUMN "motivation" TYPE TEXT COLLATE "und-x-icu";
ALTER TABLE "proposal_contents" ALTER COLUMN "rationale" TYPE TEXT COLLATE "und-x-icu";
ALTER TABLE "comments" ALTER COLUMN "text" TYPE TEXT COLLATE "und-x-icu";

-- Users (the public display name; `username` is lowercase hex and stays as is)
ALTER TABLE "users" ALTER COLUMN "govtool_username" TYPE VARCHAR(30) COLLATE "und-x-icu";

-- Budget discussions
ALTER TABLE "bds" ALTER COLUMN "intersect_admin_further_text" TYPE TEXT COLLATE "und-x-icu";
ALTER TABLE "bd_proposal_details" ALTER COLUMN "proposal_name" TYPE TEXT COLLATE "und-x-icu";

-- Lookup names (sortable on their list routes)
ALTER TABLE "governance_action_types" ALTER COLUMN "name" TYPE VARCHAR(80) COLLATE "und-x-icu";
ALTER TABLE "bd_types" ALTER COLUMN "type_name" TYPE VARCHAR(255) COLLATE "und-x-icu";
ALTER TABLE "bd_road_maps" ALTER COLUMN "roadmap_name" TYPE VARCHAR(255) COLLATE "und-x-icu";
ALTER TABLE "bd_intersect_committees" ALTER COLUMN "committee_name" TYPE VARCHAR(255) COLLATE "und-x-icu";
ALTER TABLE "bd_contract_types" ALTER COLUMN "contract_type_name" TYPE VARCHAR(255) COLLATE "und-x-icu";
ALTER TABLE "bd_currency_lists" ALTER COLUMN "currency_name" TYPE VARCHAR(255) COLLATE "und-x-icu";
ALTER TABLE "country_lists" ALTER COLUMN "country_name" TYPE VARCHAR(255) COLLATE "und-x-icu";
