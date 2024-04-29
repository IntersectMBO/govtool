import {
  Anchor,
  AnchorDataHash,
  URL,
} from "@emurgo/cardano-serialization-lib-asmjs";

export const generateAnchor = (url: string, hash: string) => {
  const metadataUrl = URL.new(url);
  const urlHash = AnchorDataHash.from_hex(hash);
  const anchor = Anchor.new(metadataUrl, urlHash);

  return anchor;
};
