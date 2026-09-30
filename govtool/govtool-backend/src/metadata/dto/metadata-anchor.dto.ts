import { IsString, Matches, MaxLength } from 'class-validator';

/** A blake2b-256 digest: exactly 64 hex characters. */
export const HASH_PATTERN = /^[0-9a-fA-F]{64}$/;

/**
 * The schemes an anchor may use. The metadata service applies its own
 * address guard (D122); this only keeps obvious junk off the private network.
 */
export const ANCHOR_URL_PATTERN = /^(https?|ipfs):\/\/\S+$/i;

/** Report ids are opaque tokens, restricted so one is always safe in a path. */
export const REPORT_ID_PATTERN = /^[A-Za-z0-9-]{1,64}$/;

/** Cardano caps an anchor url at 128 bytes; this leaves room to spare. */
const MAX_URL_LENGTH = 2048;

/** `(url, hash)`: the query of resolve and report history, and the retry body. */
export class MetadataAnchorDto {
  @IsString()
  @Matches(HASH_PATTERN, { message: 'hash must be 64 hex characters' })
  hash!: string;

  @IsString()
  @MaxLength(MAX_URL_LENGTH)
  @Matches(ANCHOR_URL_PATTERN, {
    message: 'url must start with http://, https:// or ipfs://',
  })
  url!: string;
}

export class MetadataReportIdDto {
  @IsString()
  @Matches(REPORT_ID_PATTERN, { message: 'id is not a valid report id' })
  id!: string;
}
