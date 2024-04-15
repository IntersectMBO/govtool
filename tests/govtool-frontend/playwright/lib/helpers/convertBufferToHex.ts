export default function convertBufferToHex(buffer: Uint8Array) {
  return Buffer.from(buffer).toString("hex");
}
