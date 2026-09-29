-- CreateSchema
CREATE SCHEMA IF NOT EXISTS "public";

-- CreateTable
CREATE TABLE "users" (
    "id" SERIAL NOT NULL,
    "username" VARCHAR(58) NOT NULL,
    "govtool_username" VARCHAR(30),
    "is_validated" BOOLEAN NOT NULL DEFAULT false,
    "blocked" BOOLEAN NOT NULL DEFAULT false,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "users_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "auth_challenges" (
    "id" SERIAL NOT NULL,
    "identifier" VARCHAR(58) NOT NULL,
    "nonce" CHAR(32) NOT NULL,
    "message" TEXT NOT NULL,
    "timestamp" TIMESTAMPTZ(3) NOT NULL,
    "expires_at" TIMESTAMPTZ(3) NOT NULL,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "auth_challenges_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "governance_action_types" (
    "id" SERIAL NOT NULL,
    "name" VARCHAR(80) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "governance_action_types_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "proposals" (
    "id" SERIAL NOT NULL,
    "user_id" INTEGER NOT NULL,
    "likes" INTEGER NOT NULL DEFAULT 0,
    "dislikes" INTEGER NOT NULL DEFAULT 0,
    "comments_number" INTEGER NOT NULL DEFAULT 0,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "proposals_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "proposal_contents" (
    "id" SERIAL NOT NULL,
    "proposal_id" INTEGER NOT NULL,
    "user_id" INTEGER NOT NULL,
    "gov_action_type_id" INTEGER NOT NULL,
    "name" VARCHAR(80) NOT NULL,
    "abstract" TEXT NOT NULL DEFAULT '',
    "motivation" TEXT NOT NULL DEFAULT '',
    "rationale" TEXT NOT NULL DEFAULT '',
    "rev_active" BOOLEAN NOT NULL DEFAULT false,
    "is_draft" BOOLEAN NOT NULL DEFAULT false,
    "submitted" BOOLEAN NOT NULL DEFAULT false,
    "is_locked" BOOLEAN NOT NULL DEFAULT false,
    "submission_tx_hash" CHAR(64),
    "submission_date" DATE,
    "hard_fork_content_id" INTEGER,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "proposal_contents_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "proposal_links" (
    "id" SERIAL NOT NULL,
    "content_id" INTEGER NOT NULL,
    "position" INTEGER NOT NULL,
    "link" VARCHAR(2048) NOT NULL,
    "text" VARCHAR(255),

    CONSTRAINT "proposal_links_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "proposal_withdrawals" (
    "id" SERIAL NOT NULL,
    "content_id" INTEGER NOT NULL,
    "position" INTEGER NOT NULL,
    "receiving_address" VARCHAR(200),
    "amount" DOUBLE PRECISION,

    CONSTRAINT "proposal_withdrawals_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "proposal_constitution_contents" (
    "id" SERIAL NOT NULL,
    "content_id" INTEGER NOT NULL,
    "constitution_url" VARCHAR(2048),
    "have_guardrails_script" BOOLEAN,
    "guardrails_script_url" VARCHAR(2048),
    "guardrails_script_hash" VARCHAR(255),
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "proposal_constitution_contents_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "proposal_hard_fork_contents" (
    "id" SERIAL NOT NULL,
    "previous_ga_hash" VARCHAR(255),
    "previous_ga_id" VARCHAR(255),
    "major" VARCHAR(255),
    "minor" VARCHAR(255),
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "proposal_hard_fork_contents_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "proposal_votes" (
    "id" SERIAL NOT NULL,
    "proposal_id" INTEGER NOT NULL,
    "user_id" INTEGER NOT NULL,
    "vote_result" BOOLEAN NOT NULL,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "proposal_votes_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "polls" (
    "id" SERIAL NOT NULL,
    "proposal_id" INTEGER NOT NULL,
    "yes" INTEGER NOT NULL DEFAULT 0,
    "no" INTEGER NOT NULL DEFAULT 0,
    "start_dt" TIMESTAMPTZ(3),
    "is_active" BOOLEAN NOT NULL DEFAULT false,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "polls_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "poll_votes" (
    "id" SERIAL NOT NULL,
    "poll_id" INTEGER NOT NULL,
    "user_id" INTEGER NOT NULL,
    "vote_result" BOOLEAN NOT NULL,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "poll_votes_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "comments" (
    "id" SERIAL NOT NULL,
    "proposal_id" INTEGER,
    "bd_master_id" INTEGER,
    "parent_id" INTEGER,
    "user_id" INTEGER NOT NULL,
    "text" TEXT NOT NULL,
    "drep_id" CHAR(56),
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "comments_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "comments_reports" (
    "id" SERIAL NOT NULL,
    "comment_id" INTEGER NOT NULL,
    "reporter_id" INTEGER NOT NULL,
    "moderator_id" INTEGER,
    "moderation_status" BOOLEAN,
    "hash" CHAR(89) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "comments_reports_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bds" (
    "id" SERIAL NOT NULL,
    "creator_id" INTEGER NOT NULL,
    "master_id" INTEGER,
    "is_active" BOOLEAN NOT NULL DEFAULT true,
    "privacy_policy" BOOLEAN NOT NULL,
    "intersect_named_administrator" BOOLEAN NOT NULL DEFAULT false,
    "intersect_admin_further_text" TEXT,
    "comments_number" INTEGER NOT NULL DEFAULT 0,
    "submitted_for_vote" TIMESTAMPTZ(3),
    "costing_id" INTEGER,
    "proposal_detail_id" INTEGER,
    "psapb_id" INTEGER,
    "proposal_ownership_id" INTEGER,
    "further_information_id" INTEGER,
    "contact_information_id" INTEGER,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bds_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_costings" (
    "id" SERIAL NOT NULL,
    "cost_breakdown" TEXT,
    "preferred_currency_id" INTEGER,
    "ada_amount" VARCHAR(255),
    "amount_in_preferred_currency" VARCHAR(255),
    "usd_to_ada_conversion_rate" VARCHAR(255),
    "ada_amount_clone" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "amount_in_preferred_currency_clone" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "usd_to_ada_conversion_rate_clone" DOUBLE PRECISION NOT NULL DEFAULT 0,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_costings_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_proposal_details" (
    "id" SERIAL NOT NULL,
    "proposal_name" TEXT,
    "proposal_description" TEXT,
    "key_dependencies" TEXT,
    "maintain_and_support" TEXT,
    "key_proposal_deliverables" TEXT,
    "resourcing_duration_estimates" TEXT,
    "experience" TEXT,
    "other_contract_type" TEXT,
    "contract_type_id" INTEGER,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_proposal_details_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_psapbs" (
    "id" SERIAL NOT NULL,
    "problem_statement" TEXT,
    "proposal_benefit" TEXT,
    "supplementary_endorsement" TEXT,
    "explain_proposal_roadmap" TEXT,
    "type_id" INTEGER,
    "roadmap_id" INTEGER,
    "committee_id" INTEGER,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_psapbs_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_proposal_ownerships" (
    "id" SERIAL NOT NULL,
    "agreed" BOOLEAN,
    "group_name" VARCHAR(255),
    "company_name" VARCHAR(255),
    "type_of_group" VARCHAR(255),
    "social_handles" VARCHAR(255),
    "submited_on_behalf" VARCHAR(255),
    "company_domain_name" VARCHAR(255),
    "proposal_public_champion" VARCHAR(255),
    "key_info_to_identify_group" TEXT,
    "be_country_id" INTEGER,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_proposal_ownerships_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_further_informations" (
    "id" SERIAL NOT NULL,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_further_informations_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_links" (
    "id" SERIAL NOT NULL,
    "further_information_id" INTEGER NOT NULL,
    "position" INTEGER NOT NULL,
    "link" VARCHAR(2048) NOT NULL,
    "text" VARCHAR(255),

    CONSTRAINT "bd_links_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_contact_informations" (
    "id" SERIAL NOT NULL,
    "be_full_name" VARCHAR(255),
    "be_email" VARCHAR(255),
    "submission_lead_full_name" VARCHAR(255),
    "submission_lead_email" VARCHAR(255),
    "other_contract_type" VARCHAR(255),
    "be_country_of_res_id" INTEGER,
    "be_nationality_id" INTEGER,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_contact_informations_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_polls" (
    "id" SERIAL NOT NULL,
    "bd_master_id" INTEGER NOT NULL,
    "yes" INTEGER NOT NULL DEFAULT 0,
    "no" INTEGER NOT NULL DEFAULT 0,
    "is_active" BOOLEAN NOT NULL DEFAULT true,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_polls_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_poll_votes" (
    "id" SERIAL NOT NULL,
    "bd_poll_id" INTEGER NOT NULL,
    "user_id" INTEGER NOT NULL,
    "vote_result" BOOLEAN NOT NULL,
    "drep_id" CHAR(56) NOT NULL,
    "drep_voting_power" VARCHAR(255) NOT NULL,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_poll_votes_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_drafts" (
    "id" SERIAL NOT NULL,
    "creator_id" INTEGER NOT NULL,
    "draft_data" JSONB NOT NULL,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_drafts_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_types" (
    "id" SERIAL NOT NULL,
    "type_name" VARCHAR(255) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_types_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_road_maps" (
    "id" SERIAL NOT NULL,
    "roadmap_name" VARCHAR(255) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_road_maps_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_intersect_committees" (
    "id" SERIAL NOT NULL,
    "committee_name" VARCHAR(255) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_intersect_committees_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_contract_types" (
    "id" SERIAL NOT NULL,
    "contract_type_name" VARCHAR(255) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_contract_types_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "bd_currency_lists" (
    "id" SERIAL NOT NULL,
    "currency_name" VARCHAR(255) NOT NULL,
    "currency_letter_code" VARCHAR(255) NOT NULL,
    "currency_number_code" VARCHAR(255) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "bd_currency_lists_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "country_lists" (
    "id" SERIAL NOT NULL,
    "country_name" VARCHAR(255) NOT NULL,
    "alfa_2_code" VARCHAR(255) NOT NULL,
    "alfa_3_code" VARCHAR(255) NOT NULL,
    "published_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "created_at" TIMESTAMPTZ(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMPTZ(3) NOT NULL,

    CONSTRAINT "country_lists_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "users_username_key" ON "users"("username");

-- CreateIndex
CREATE UNIQUE INDEX "users_govtool_username_key" ON "users"("govtool_username");

-- CreateIndex
CREATE UNIQUE INDEX "auth_challenges_nonce_key" ON "auth_challenges"("nonce");

-- CreateIndex
CREATE INDEX "auth_challenges_identifier_idx" ON "auth_challenges"("identifier");

-- CreateIndex
CREATE INDEX "auth_challenges_expires_at_idx" ON "auth_challenges"("expires_at");

-- CreateIndex
CREATE INDEX "proposals_user_id_idx" ON "proposals"("user_id");

-- CreateIndex
CREATE UNIQUE INDEX "proposal_contents_submission_tx_hash_key" ON "proposal_contents"("submission_tx_hash");

-- CreateIndex
CREATE UNIQUE INDEX "proposal_contents_hard_fork_content_id_key" ON "proposal_contents"("hard_fork_content_id");

-- CreateIndex
CREATE INDEX "proposal_contents_proposal_id_rev_active_is_draft_idx" ON "proposal_contents"("proposal_id", "rev_active", "is_draft");

-- CreateIndex
CREATE INDEX "proposal_contents_user_id_is_draft_idx" ON "proposal_contents"("user_id", "is_draft");

-- CreateIndex
CREATE INDEX "proposal_contents_gov_action_type_id_idx" ON "proposal_contents"("gov_action_type_id");

-- CreateIndex
CREATE INDEX "proposal_links_content_id_position_idx" ON "proposal_links"("content_id", "position");

-- CreateIndex
CREATE INDEX "proposal_withdrawals_content_id_position_idx" ON "proposal_withdrawals"("content_id", "position");

-- CreateIndex
CREATE UNIQUE INDEX "proposal_constitution_contents_content_id_key" ON "proposal_constitution_contents"("content_id");

-- CreateIndex
CREATE INDEX "proposal_votes_user_id_idx" ON "proposal_votes"("user_id");

-- CreateIndex
CREATE UNIQUE INDEX "proposal_votes_proposal_id_user_id_key" ON "proposal_votes"("proposal_id", "user_id");

-- CreateIndex
CREATE INDEX "polls_proposal_id_is_active_created_at_idx" ON "polls"("proposal_id", "is_active", "created_at");

-- CreateIndex
CREATE INDEX "poll_votes_user_id_idx" ON "poll_votes"("user_id");

-- CreateIndex
CREATE UNIQUE INDEX "poll_votes_poll_id_user_id_key" ON "poll_votes"("poll_id", "user_id");

-- CreateIndex
CREATE INDEX "comments_proposal_id_parent_id_created_at_idx" ON "comments"("proposal_id", "parent_id", "created_at");

-- CreateIndex
CREATE INDEX "comments_bd_master_id_parent_id_created_at_idx" ON "comments"("bd_master_id", "parent_id", "created_at");

-- CreateIndex
CREATE INDEX "comments_parent_id_idx" ON "comments"("parent_id");

-- CreateIndex
CREATE INDEX "comments_user_id_idx" ON "comments"("user_id");

-- CreateIndex
CREATE UNIQUE INDEX "comments_reports_hash_key" ON "comments_reports"("hash");

-- CreateIndex
CREATE INDEX "comments_reports_reporter_id_idx" ON "comments_reports"("reporter_id");

-- CreateIndex
CREATE UNIQUE INDEX "comments_reports_comment_id_reporter_id_key" ON "comments_reports"("comment_id", "reporter_id");

-- CreateIndex
CREATE UNIQUE INDEX "bds_costing_id_key" ON "bds"("costing_id");

-- CreateIndex
CREATE UNIQUE INDEX "bds_proposal_detail_id_key" ON "bds"("proposal_detail_id");

-- CreateIndex
CREATE UNIQUE INDEX "bds_psapb_id_key" ON "bds"("psapb_id");

-- CreateIndex
CREATE UNIQUE INDEX "bds_proposal_ownership_id_key" ON "bds"("proposal_ownership_id");

-- CreateIndex
CREATE UNIQUE INDEX "bds_further_information_id_key" ON "bds"("further_information_id");

-- CreateIndex
CREATE UNIQUE INDEX "bds_contact_information_id_key" ON "bds"("contact_information_id");

-- CreateIndex
CREATE INDEX "bds_master_id_created_at_idx" ON "bds"("master_id", "created_at");

-- CreateIndex
CREATE INDEX "bds_is_active_created_at_idx" ON "bds"("is_active", "created_at");

-- CreateIndex
CREATE INDEX "bds_creator_id_idx" ON "bds"("creator_id");

-- CreateIndex
CREATE INDEX "bd_psapbs_type_id_idx" ON "bd_psapbs"("type_id");

-- CreateIndex
CREATE INDEX "bd_links_further_information_id_position_idx" ON "bd_links"("further_information_id", "position");

-- CreateIndex
CREATE INDEX "bd_polls_bd_master_id_is_active_created_at_idx" ON "bd_polls"("bd_master_id", "is_active", "created_at");

-- CreateIndex
CREATE INDEX "bd_poll_votes_user_id_idx" ON "bd_poll_votes"("user_id");

-- CreateIndex
CREATE UNIQUE INDEX "bd_poll_votes_bd_poll_id_user_id_key" ON "bd_poll_votes"("bd_poll_id", "user_id");

-- CreateIndex
CREATE UNIQUE INDEX "bd_poll_votes_bd_poll_id_drep_id_key" ON "bd_poll_votes"("bd_poll_id", "drep_id");

-- CreateIndex
CREATE INDEX "bd_drafts_creator_id_idx" ON "bd_drafts"("creator_id");

-- AddForeignKey
ALTER TABLE "proposals" ADD CONSTRAINT "proposals_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_contents" ADD CONSTRAINT "proposal_contents_proposal_id_fkey" FOREIGN KEY ("proposal_id") REFERENCES "proposals"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_contents" ADD CONSTRAINT "proposal_contents_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_contents" ADD CONSTRAINT "proposal_contents_gov_action_type_id_fkey" FOREIGN KEY ("gov_action_type_id") REFERENCES "governance_action_types"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_contents" ADD CONSTRAINT "proposal_contents_hard_fork_content_id_fkey" FOREIGN KEY ("hard_fork_content_id") REFERENCES "proposal_hard_fork_contents"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_links" ADD CONSTRAINT "proposal_links_content_id_fkey" FOREIGN KEY ("content_id") REFERENCES "proposal_contents"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_withdrawals" ADD CONSTRAINT "proposal_withdrawals_content_id_fkey" FOREIGN KEY ("content_id") REFERENCES "proposal_contents"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_constitution_contents" ADD CONSTRAINT "proposal_constitution_contents_content_id_fkey" FOREIGN KEY ("content_id") REFERENCES "proposal_contents"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_votes" ADD CONSTRAINT "proposal_votes_proposal_id_fkey" FOREIGN KEY ("proposal_id") REFERENCES "proposals"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "proposal_votes" ADD CONSTRAINT "proposal_votes_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "polls" ADD CONSTRAINT "polls_proposal_id_fkey" FOREIGN KEY ("proposal_id") REFERENCES "proposals"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "poll_votes" ADD CONSTRAINT "poll_votes_poll_id_fkey" FOREIGN KEY ("poll_id") REFERENCES "polls"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "poll_votes" ADD CONSTRAINT "poll_votes_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "comments" ADD CONSTRAINT "comments_proposal_id_fkey" FOREIGN KEY ("proposal_id") REFERENCES "proposals"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "comments" ADD CONSTRAINT "comments_bd_master_id_fkey" FOREIGN KEY ("bd_master_id") REFERENCES "bds"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "comments" ADD CONSTRAINT "comments_parent_id_fkey" FOREIGN KEY ("parent_id") REFERENCES "comments"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "comments" ADD CONSTRAINT "comments_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "comments_reports" ADD CONSTRAINT "comments_reports_comment_id_fkey" FOREIGN KEY ("comment_id") REFERENCES "comments"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "comments_reports" ADD CONSTRAINT "comments_reports_reporter_id_fkey" FOREIGN KEY ("reporter_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "comments_reports" ADD CONSTRAINT "comments_reports_moderator_id_fkey" FOREIGN KEY ("moderator_id") REFERENCES "users"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_creator_id_fkey" FOREIGN KEY ("creator_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_master_id_fkey" FOREIGN KEY ("master_id") REFERENCES "bds"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_costing_id_fkey" FOREIGN KEY ("costing_id") REFERENCES "bd_costings"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_proposal_detail_id_fkey" FOREIGN KEY ("proposal_detail_id") REFERENCES "bd_proposal_details"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_psapb_id_fkey" FOREIGN KEY ("psapb_id") REFERENCES "bd_psapbs"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_proposal_ownership_id_fkey" FOREIGN KEY ("proposal_ownership_id") REFERENCES "bd_proposal_ownerships"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_further_information_id_fkey" FOREIGN KEY ("further_information_id") REFERENCES "bd_further_informations"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bds" ADD CONSTRAINT "bds_contact_information_id_fkey" FOREIGN KEY ("contact_information_id") REFERENCES "bd_contact_informations"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_costings" ADD CONSTRAINT "bd_costings_preferred_currency_id_fkey" FOREIGN KEY ("preferred_currency_id") REFERENCES "bd_currency_lists"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_proposal_details" ADD CONSTRAINT "bd_proposal_details_contract_type_id_fkey" FOREIGN KEY ("contract_type_id") REFERENCES "bd_contract_types"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_psapbs" ADD CONSTRAINT "bd_psapbs_type_id_fkey" FOREIGN KEY ("type_id") REFERENCES "bd_types"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_psapbs" ADD CONSTRAINT "bd_psapbs_roadmap_id_fkey" FOREIGN KEY ("roadmap_id") REFERENCES "bd_road_maps"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_psapbs" ADD CONSTRAINT "bd_psapbs_committee_id_fkey" FOREIGN KEY ("committee_id") REFERENCES "bd_intersect_committees"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_proposal_ownerships" ADD CONSTRAINT "bd_proposal_ownerships_be_country_id_fkey" FOREIGN KEY ("be_country_id") REFERENCES "country_lists"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_links" ADD CONSTRAINT "bd_links_further_information_id_fkey" FOREIGN KEY ("further_information_id") REFERENCES "bd_further_informations"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_contact_informations" ADD CONSTRAINT "bd_contact_informations_be_country_of_res_id_fkey" FOREIGN KEY ("be_country_of_res_id") REFERENCES "country_lists"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_contact_informations" ADD CONSTRAINT "bd_contact_informations_be_nationality_id_fkey" FOREIGN KEY ("be_nationality_id") REFERENCES "country_lists"("id") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_polls" ADD CONSTRAINT "bd_polls_bd_master_id_fkey" FOREIGN KEY ("bd_master_id") REFERENCES "bds"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_poll_votes" ADD CONSTRAINT "bd_poll_votes_bd_poll_id_fkey" FOREIGN KEY ("bd_poll_id") REFERENCES "bd_polls"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_poll_votes" ADD CONSTRAINT "bd_poll_votes_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "bd_drafts" ADD CONSTRAINT "bd_drafts_creator_id_fkey" FOREIGN KEY ("creator_id") REFERENCES "users"("id") ON DELETE CASCADE ON UPDATE CASCADE;


-- Constraints Prisma cannot declare (SPEC §5). Keep them when regenerating.

-- At most one active poll per proposal, one active version per BD chain, one
-- active poll per BD.
CREATE UNIQUE INDEX "polls_proposal_id_active_key" ON "polls" ("proposal_id") WHERE "is_active";
CREATE UNIQUE INDEX "bds_master_id_active_key" ON "bds" ("master_id") WHERE "is_active";
CREATE UNIQUE INDEX "bd_polls_bd_master_id_active_key" ON "bd_polls" ("bd_master_id") WHERE "is_active";

-- A comment targets exactly one of a proposal or a BD master row.
ALTER TABLE "comments" ADD CONSTRAINT "comments_target_check"
  CHECK (("proposal_id" IS NULL) <> ("bd_master_id" IS NULL));
