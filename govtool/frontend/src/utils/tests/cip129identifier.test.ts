import {
  encodeCIP129Identifier,
  decodeCIP129Identifier,
} from "../cip129identifier";

const txID = "0e52f799df70b3b74e57d3c7c89f5d44b6deffb3b8b3a718abf9bb41d33e3a92";
const index = "00";
const bech32Prefix = "gov_action";

describe("CIP-129 Governance Identifier", () => {
  describe("encodeCIP129Identifier", () => {
    it("should encode a CIP129 identifier correctly", () => {
      const result = encodeCIP129Identifier(txID, index, bech32Prefix);
      expect(result).toBe(
        "gov_action1pef00xwlwzemwnjh60ru386agjmdalanhze6wx9tlxa5r5e782fqqt7spu6",
      );
    });
  });

  describe("decodeCIP129Identifier", () => {
    it("should decode a CIP129 identifier correctly", () => {
      const result = decodeCIP129Identifier(
        encodeCIP129Identifier(txID, index, bech32Prefix),
      );
      expect(result).toEqual({
        txID,
        index,
        prefix: bech32Prefix,
      });
    });
  });
});
