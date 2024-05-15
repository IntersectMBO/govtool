import environments from "@constants/environments";
import { Logger } from "@helpers/logger";

import fetch = require("node-fetch");

const metadataBucketService = {
  uploadMetadata: async (name: string, data: JSON) => {
    try {
      const res = await fetch(`${environments.metadataBucketUrl}/${name}`, {
        method: "PUT",
        body: JSON.stringify(data),
      });
      Logger.success(`Uploaded ${name} metadata to bucket`);
      return `${environments.metadataBucketUrl}/${name}`;
    } catch (err) {
      Logger.fail(`Failed to upload ${name} metadata: ${err}`);
      throw err;
    }
  },
};

export default metadataBucketService;
