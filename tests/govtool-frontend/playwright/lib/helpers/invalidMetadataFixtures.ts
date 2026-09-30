import environments from "@constants/environments";
import metadataBucketService from "@services/metadataBucketService";
import { blake2bHex } from "blakejs";
import { randomBytes } from "crypto";
import * as fs from "fs";
import * as path from "path";

/**
 * Anchors for the invalid-metadata cases (4P, 9H). The documents are committed
 * under lib/_mock/metadata-fixtures and uploaded to the configured bucket by
 * ensureInvalidMetadataFixtures(), so no case depends on content someone else
 * deployed. Hashes are computed from the files, never written down.
 */

const FIXTURE_DIR = path.resolve(__dirname, "../_mock/metadata-fixtures");

const hashOf = (content: string) =>
  blake2bHex(Buffer.from(content, "utf8"), undefined, 32);

export interface MetadataFixture {
  content: string;
  hash: string;
  /** Object name in the bucket: content-addressed, so every run and every
   * environment sharing a bucket writes the same bytes under it. */
  name: string;
  url: string;
}

const loadFixture = (file: string): MetadataFixture => {
  const content = fs.readFileSync(path.join(FIXTURE_DIR, file), "utf8");
  const hash = hashOf(content);
  const name = `playwright-${hash.slice(0, 16)}-${file}`;
  return {
    content,
    hash,
    name,
    url: `${environments.metadataBucketUrl}/${name}`,
  };
};

/** Valid JSON with no CIP-108 body. */
export const incorrectFormatFixture = loadFixture("incorrect-format.json");

/** A valid CIP-108 document. */
export const validCip108Fixture = loadFixture("valid-cip108.jsonld");

/**
 * A hash no document has: the metadata service treats content cached under a
 * hash as authoritative whatever the url, so a mismatch case must never use
 * the hash of a real document (another fixture's included). Its preimage is
 * this string, which is never uploaded.
 */
export const UNMATCHED_METADATA_HASH = hashOf(
  "govtool playwright: the preimage of this hash is never served"
);

/** A bucket url nothing is uploaded to; unique so no cached failure is reused. */
export const missingMetadataUrl = `${environments.metadataBucketUrl}/playwright-missing-${randomBytes(8).toString("hex")}.jsonld`;

const ensureFixture = async (fixture: MetadataFixture) => {
  const stored = await metadataBucketService.getRaw(fixture.name);
  if (stored !== undefined && hashOf(stored) === fixture.hash) return;

  await metadataBucketService.uploadRaw(fixture.name, fixture.content);
  const served = await metadataBucketService.getRaw(fixture.name);
  if (served === undefined || hashOf(served) !== fixture.hash) {
    throw new Error(
      `The bucket does not serve ${fixture.url} byte for byte; its hash would not match ${fixture.hash}`
    );
  }
};

let uploaded: Promise<void> | undefined;

/** Upload-if-missing, once per worker. */
export function ensureInvalidMetadataFixtures(): Promise<void> {
  uploaded ??= Promise.all(
    [incorrectFormatFixture, validCip108Fixture].map(ensureFixture)
  ).then(() => undefined);
  uploaded.catch(() => (uploaded = undefined));
  return uploaded;
}

/**
 * The constitution document the 7H submission tests anchor to. pdf-ui fetches
 * it through the pdf backend's proxy and hashes it, so any stable content
 * works; it used to be data.jsonId pre-deployed on the public bucket.
 */
export const constitutionFixture = loadFixture("constitution.jsonld");

/** Upload-if-missing; returns the constitution url for a proposal. */
export async function ensureConstitutionFixture(): Promise<string> {
  await ensureFixture(constitutionFixture);
  return constitutionFixture.url;
}
