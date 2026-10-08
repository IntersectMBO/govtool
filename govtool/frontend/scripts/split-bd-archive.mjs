#!/usr/bin/env node
// Splits the 2025 budget discussion export into the static files the
// read-only archive serves: public/budget-proposals-2025/list.json (what the
// list page needs per discussion) and one <master_id>.json per discussion
// (every version, the polls and the comments).
//
// The export (bd-archive-2025.json, made by
// govtool-pdf-backend/scripts/export-bd-archive.sh from mainnet's Strapi
// forum) is not committed; the files this writes are the archive's source of
// truth. Kept for reference and for a re-split from the same export:
//
//   node scripts/split-bd-archive.mjs path/to/bd-archive-2025.json
//
// The forum API returned every user column of a version's creator, password
// hash and tokens included. Only id and govtool_username are kept, and a
// comment loses its internal user_id.

import { mkdir, readFile, readdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const OUT_DIR = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../public/budget-proposals-2025",
);

// Strapi lookup rows carry their own timestamps; the archive has no use for
// them.
const LOOKUP_TIMESTAMPS = ["createdAt", "updatedAt", "publishedAt"];

const stripLookup = (relation) => {
  const row = relation?.data;
  if (!row?.attributes) return relation;
  const attributes = { ...row.attributes };
  LOOKUP_TIMESTAMPS.forEach((key) => delete attributes[key]);
  return { data: { id: row.id, attributes } };
};

const sanitizeCreator = (creator) => {
  const row = creator?.data;
  if (!row) return { data: null };
  return {
    data: {
      id: row.id,
      attributes: {
        govtool_username: row.attributes?.govtool_username ?? null,
      },
    },
  };
};

const withRelation = (relation, fn) =>
  relation?.data
    ? {
        data: {
          ...relation.data,
          attributes: fn({ ...relation.data.attributes }),
        },
      }
    : (relation ?? { data: null });

const sanitizeVersion = (version) => {
  const a = version.attributes;
  return {
    id: version.id,
    attributes: {
      ...a,
      creator: sanitizeCreator(a.creator),
      bd_costing: withRelation(a.bd_costing, (c) => ({
        ...c,
        preferred_currency: stripLookup(c.preferred_currency),
      })),
      bd_proposal_detail: withRelation(a.bd_proposal_detail, (d) => ({
        ...d,
        contract_type_name: stripLookup(d.contract_type_name),
      })),
      bd_psapb: withRelation(a.bd_psapb, (p) => ({
        ...p,
        type_name: stripLookup(p.type_name),
        roadmap_name: stripLookup(p.roadmap_name),
        committee_name: stripLookup(p.committee_name),
      })),
      bd_proposal_ownership: withRelation(a.bd_proposal_ownership, (o) => ({
        ...o,
        be_country: stripLookup(o.be_country),
      })),
    },
  };
};

const sanitizeComment = (comment) => {
  const { user_id: _userId, ...attributes } = comment.attributes;
  return { id: comment.id, attributes };
};

// The forum showed the newest active poll, else nothing; the archive falls
// back to the newest poll so a closed one still shows its totals.
export const pickPoll = (polls) => {
  const newestFirst = [...polls].sort((x, y) =>
    y.attributes.createdAt.localeCompare(x.attributes.createdAt),
  );
  return (
    newestFirst.find((p) => p.attributes.is_poll_active) ??
    newestFirst[0] ??
    null
  );
};

// The card clamps the benefit to three lines; the full text is in the
// discussion's own file.
const CARD_BENEFIT_LENGTH = 300;
export const cardExcerpt = (text) => {
  if (!text || text.length <= CARD_BENEFIT_LENGTH) return text ?? "";
  const cut = text.slice(0, CARD_BENEFIT_LENGTH);
  return `${cut.slice(0, cut.lastIndexOf(" ") + 1 || undefined).trimEnd()}…`;
};

export const activeVersion = (versions) =>
  versions.find((v) => v.attributes.is_active) ?? versions[0];

const listItem = (bd, versions, poll) => {
  const active = activeVersion(versions).attributes;
  const oldest = versions.reduce((min, v) =>
    v.attributes.createdAt < min.attributes.createdAt ? v : min,
  );
  const costing = active.bd_costing?.data?.attributes;
  const psapb = active.bd_psapb?.data?.attributes;
  return {
    id: activeVersion(versions).id,
    attributes: {
      master_id: bd.master_id,
      createdAt: active.createdAt,
      master_proposal_created_at: oldest.attributes.createdAt,
      submitted_for_vote: active.submitted_for_vote,
      prop_comments_number: bd.comments.length,
      creator: active.creator,
      bd_costing: {
        data: {
          attributes: {
            ada_amount: costing?.ada_amount ?? null,
            amount_in_preferred_currency:
              costing?.amount_in_preferred_currency ?? null,
            preferred_currency: costing?.preferred_currency ?? { data: null },
          },
        },
      },
      bd_psapb: {
        data: {
          attributes: {
            proposal_benefit: cardExcerpt(psapb?.proposal_benefit),
            type_name: psapb?.type_name ?? { data: null },
          },
        },
      },
      bd_proposal_detail: {
        data: {
          attributes: {
            proposal_name:
              active.bd_proposal_detail?.data?.attributes?.proposal_name ?? "",
          },
        },
      },
    },
    archive: {
      versions: versions.length,
      poll: poll
        ? { yes: +poll.attributes.poll_yes, no: +poll.attributes.poll_no }
        : null,
    },
  };
};

const main = async () => {
  const input = process.argv[2];
  if (!input) {
    console.error("usage: node scripts/split-bd-archive.mjs <bd-archive.json>");
    process.exit(1);
  }
  const archive = JSON.parse(await readFile(input, "utf8"));
  if (archive.bds.length !== archive.count) {
    throw new Error(`count ${archive.count} != ${archive.bds.length} bds`);
  }

  await mkdir(OUT_DIR, { recursive: true });
  for (const file of await readdir(OUT_DIR)) {
    if (file.endsWith(".json")) await rm(path.join(OUT_DIR, file));
  }

  const categories = new Map();
  const items = [];
  for (const bd of archive.bds) {
    if (!/^\d+$/.test(bd.master_id)) {
      throw new Error(`unexpected master_id ${bd.master_id}`);
    }
    const versions = bd.versions.map(sanitizeVersion);
    const comments = bd.comments.map(sanitizeComment);
    const poll = pickPoll(bd.polls);
    const item = listItem({ ...bd, comments }, versions, poll);
    items.push(item);

    const type = item.attributes.bd_psapb.data.attributes.type_name.data;
    if (type) categories.set(type.id, type);

    await writeFile(
      path.join(OUT_DIR, `${bd.master_id}.json`),
      JSON.stringify({
        master_id: bd.master_id,
        versions,
        polls: bd.polls,
        comments,
      }),
    );
  }

  await writeFile(
    path.join(OUT_DIR, "list.json"),
    JSON.stringify({
      generatedAt: archive.generatedAt,
      source: archive.source,
      count: items.length,
      categories: [...categories.values()].sort((x, y) => x.id - y.id),
      items,
    }),
  );

  const totals = archive.bds.reduce(
    (t, bd) => ({
      versions: t.versions + bd.versions.length,
      polls: t.polls + bd.polls.length,
      comments: t.comments + bd.comments.length,
    }),
    { versions: 0, polls: 0, comments: 0 },
  );
  console.log(
    `wrote ${items.length} discussions (${totals.versions} versions, ` +
      `${totals.polls} polls, ${totals.comments} comments) to ${OUT_DIR}`,
  );
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  await main();
}
