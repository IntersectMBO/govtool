import { blake2bHex } from "blakejs";
import { cborxDecoder, cborxEncoder } from "./cborEncodeDecode";

export default function computeTxHash(tx: string) {
  let decodedTx = cborxDecoder.decode(Buffer.from(tx, "hex"));
  const txBody = Uint8Array.from(cborxEncoder.encode(decodedTx[0]));
  return blake2bHex(txBody, undefined, 32);
}
