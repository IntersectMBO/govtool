import * as jsonld from "jsonld";

import { CIP_100, CIP_108 } from "@/consts";

/**
 * Generates a JSON-LD document by compacting the given body and context.
 *
 * @template T - The type of the body.
 * @template C - The type of the context.
 * @param {T} body - The body of the JSON-LD document.
 * @param {C} context - The context of the JSON-LD document.
 * @returns {Promise<any>} - A promise that resolves to the compacted JSON-LD document.
 */
export const generateJsonld = async <
  T extends Record<string, JSONValue>,
  C extends jsonld.ContextDefinition,
>(
  body: T,
  context: C,
) => {
  const doc = {
    [`${CIP_108}body`]: body,
    [`${CIP_100}hashAlgorithm`]: "blake2b-256",
    [`${CIP_100}authors`]: [],
  };

  const json = await jsonld.compact(doc, context);

  return json;
};
