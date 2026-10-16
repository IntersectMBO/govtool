import environments from "@constants/environments";
import { Logger } from "@helpers/logger";

import fetch = require("node-fetch");

const metadataBucketService = {
  uploadMetadata: async (name: string, data: JSON) => {
    try {
      const res = await fetch(`${environments.metadataBucketUrl}/${name}`, {
        method: "PUT",
        body: JSON.stringify(data, null, 2),
      });
      Logger.success(`Uploaded ${name} metadata to bucket`);
      return `${environments.metadataBucketUrl}/${name}`;
    } catch (err) {
      Logger.fail(`Failed to upload ${name} metadata: ${err}`);
      throw err;
    }
  },

  /**
   * Stores `content` as is, so a hash computed over it stays valid. The
   * bucket parses only text/plain bodies; a string body is sent as UTF-8.
   */
  uploadRaw: async (name: string, content: string) => {
    const url = `${environments.metadataBucketUrl}/${name}`;
    const res = await fetch(url, {
      method: "PUT",
      headers: { "Content-Type": "text/plain; charset=utf-8" },
      body: content,
    });
    if (!res.ok) {
      throw new Error(`Uploading ${url} failed: HTTP ${res.status}`);
    }
    return url;
  },

  /** The stored content, or undefined when the bucket has none. */
  getRaw: async (name: string): Promise<string | undefined> => {
    const res = await fetch(`${environments.metadataBucketUrl}/${name}`);
    if (res.status === 404) return undefined;
    if (!res.ok) {
      throw new Error(`Reading ${name} from the bucket failed: HTTP ${res.status}`);
    }
    return res.text();
  },
};

export default metadataBucketService;
