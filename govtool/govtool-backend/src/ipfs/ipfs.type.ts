export type UploadResponse = {
  ipfsCid: string;
};

export type PinataUploadResponse = {
  data?: {
    cid?: string;
  };
};
