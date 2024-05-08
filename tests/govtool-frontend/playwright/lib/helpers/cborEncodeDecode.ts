import { Decoder, Encoder } from "cbor-x";

export const cborxEncoder = new Encoder({
  mapsAsObjects: false,
  useRecords: false,
});
export const cborxDecoder = new Decoder({ mapsAsObjects: false });
