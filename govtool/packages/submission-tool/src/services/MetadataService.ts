import * as jsonld from "jsonld";
import { blake2bHex } from "blakejs";

import { CIP_0108_CONTEXT } from "@/consts";
import { MetadataConfig, CIP_Reference } from "@/types";
import { CIP0108ValidationSchema } from "@/schemas";

/**
 * Represents a service for handling metadata.
 */
export class MetadataService {
  public hash: string | null;
  public jsonld: jsonld.NodeObject | null;
  private cip: CIP_Reference;
  private config: MetadataConfig;

  /**
   * Constructs a new instance of the MetadataService class.
   * @param config - The metadata configuration data.
   */
  constructor(config: MetadataConfig) {
    this.config = config;
    this.hash = null;
    this.jsonld = null;
    this.cip = config.cip;
  }

  /**
   * Generates the metadata body based on the provided data.
   * @returns The generated metadata body.
   */
  private generateMetadataBody() {
    const docBody = Object.entries(this.config.body).map(([key, value]) => [
      this.config.cip + key,
      value,
    ]);

    const references = this.config.body.references.map((reference) => ({
      "@type": "Other",
      [`${this.config.cip}reference-label`]: reference.label,
      [`${this.config.cip}reference-uri`]: reference.uri,
    }));

    const body = Object.fromEntries(docBody);

    if (references) {
      body[`${this.config.cip}references`] = references;
    }

    return body;
  }

  /**
   * Generates the JSON-LD document based on the metadata configuration data.
   * @param config - The metadata configuration data.
   * @returns A promise that resolves to the MetadataService class.
   */
  private async generateJsonld() {
    const body = this.generateMetadataBody();
    const jsonLDDoc: jsonld.JsonLdDocument = {
      [`${this.cip}body`]: body,
      [`${CIP_Reference["0100"]}hashAlgorithm`]: this.config.hashAlgorithm,
      [`${CIP_Reference["0100"]}authors`]: [],
    };
    const context = this.getContext();

    this.jsonld = await jsonld.compact(jsonLDDoc, context);

    return this;
  }

  /**
   * Gets the context based on the CIP version.
   * @returns The context for the JSON-LD document.
   */
  private getContext() {
    switch (this.cip) {
      case CIP_Reference["0108"]:
        return CIP_0108_CONTEXT;
      default:
        return CIP_0108_CONTEXT;
    }
  }

  /**
   * Generates the hash for the JSON-LD document.
   * @returns A promise that resolves to the MetadataService class.
   * @throws An error if the JSON-LD document is not generated.
   */
  private async generateHash() {
    if (!this.jsonld) {
      throw new Error("JSON-LD document is not generated");
    }

    const canonizedJson = await jsonld.canonize(this.jsonld);

    this.hash = await blake2bHex(canonizedJson, undefined, 32);

    return this;
  }

  /**
   * Initializes the MetadataService.
   * @returns A promise that resolves to a new instance of the MetadataService class.
   */
  public async initialize() {
    this.validateMetadata();

    await this.generateJsonld();
    await this.generateHash();

    return this;
  }

  /**
   * Validates the metadata based on the CIP version.
   * @throws An error if the CIP version is invalid.
   */
  public validateMetadata() {
    // Only CIP 0108 is supported
    if (this.config.cip !== CIP_Reference["0108"]) {
      throw new Error("Invalid CIP version");
    }

    try {
      CIP0108ValidationSchema.parse(this.config.body);
    } catch (error) {
      throw new Error("Invalid metadata body");
    }
  }

  /**
   * Builds the metadata service with the provided configuration.
   * @param config The configuration for the metadata service.
   * @returns A promise that resolves to the built metadata service.
   */
  public async build() {
    await this.initialize();

    return this;
  }
}
