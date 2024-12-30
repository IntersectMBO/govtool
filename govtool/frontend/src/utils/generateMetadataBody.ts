type MetadataConfig = {
  data: Record<string, unknown>;
  acceptedKeys: string[];
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
}: MetadataConfig) => {
  const filteredData = Object.entries(data)
    .filter(([key, value]) => value && acceptedKeys.includes(key))
    .map(([key, value]) => [key, value]);

  const references = data?.references
    ? // uri should not be optional. It is just not yet supported on govtool
      (data.references as Array<Partial<Reference>>)
        .filter((link) => link.uri)
        .map((link) => ({
          "@type": link["@type"] ?? "Other",
          label: link.label || "Label",
          uri: link.uri,
        }))
    : undefined;

  const body = Object.fromEntries(filteredData);
  if (references?.length) {
    body.references = references;
  }

  return body;
};
