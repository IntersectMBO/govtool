import { CIP_100, CIP_108, CIP_QQQ } from "@/consts";

type StandardReference = typeof CIP_100 | typeof CIP_108 | typeof CIP_QQQ;

type MetadataConfig = {
  data: Record<string, unknown>;
  acceptedKeys: string[];
  standardReference: StandardReference;
};

/**
 * Generates the metadata body based on the provided configuration.
 *
 * @param {MetadataConfig} config - The configuration object containing
 * the data, accepted keys, and standard reference.
 * @returns {Object} - The generated metadata body.
 */
export const generateMetadataBody = ({
  data,
  acceptedKeys,
  standardReference,
}: MetadataConfig) => {
  const filteredData = Object.entries(data)
    .filter(([key]) => acceptedKeys.includes(key))
    .map(([key, value]) => [standardReference + key, value]);

  const references = data?.links
    ? // uri should not be optional. It is just not yet supported on govtool
      (data.links as Array<{ uri?: string; link: string }>)
        .filter((link) => link.link)
        .map((link) => ({
          "@type": "Other",
          [`${CIP_100}reference-label`]: link.uri || "Label",
          [`${CIP_100}reference-uri`]: link.link,
        }))
    : undefined;

  const body = Object.fromEntries(filteredData);

  if (references) {
    body[`${standardReference}references`] = references;
  }

  return body;
};
