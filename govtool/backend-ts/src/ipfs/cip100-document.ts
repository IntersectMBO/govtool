const SAFE_FILE_NAME = /[^A-Za-z0-9._-]/g;
const MAX_FILE_NAME_LENGTH = 100;
export const DEFAULT_UPLOAD_FILE_NAME = 'data.txt';

/**
 * The upload endpoint exists so users can pin CIP-100 governance metadata
 * (e.g. DRep vote rationales). Anything that is not a CIP-100 JSON-LD
 * document is rejected so the endpoint cannot be used as free file hosting.
 *
 * Returns an error message, or null when the document is acceptable.
 */
export function validateCip100Document(content: string): string | null {
  let parsed: unknown;
  try {
    parsed = JSON.parse(content);
  } catch {
    return 'The uploaded file must be a JSON-LD document';
  }

  if (!isObject(parsed)) {
    return 'The uploaded file must be a JSON-LD object';
  }

  const context = parsed['@context'];
  if (
    !(
      isObject(context) ||
      typeof context === 'string' ||
      Array.isArray(context)
    )
  ) {
    return 'The uploaded document is missing a JSON-LD @context';
  }

  if (parsed.hashAlgorithm !== 'blake2b-256') {
    return 'The uploaded document must declare hashAlgorithm blake2b-256';
  }

  if (!isObject(parsed.body)) {
    return 'The uploaded document must contain a CIP-100 body object';
  }

  if (parsed.authors !== undefined && !Array.isArray(parsed.authors)) {
    return 'The uploaded document authors must be an array';
  }

  return null;
}

export function sanitizeUploadFileName(fileName: string | undefined): string {
  const cleaned = (fileName ?? '')
    .replace(SAFE_FILE_NAME, '_')
    .replace(/^[._]+/, '')
    .slice(0, MAX_FILE_NAME_LENGTH);

  return cleaned || DEFAULT_UPLOAD_FILE_NAME;
}

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}
