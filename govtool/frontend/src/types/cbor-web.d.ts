/* eslint-disable @typescript-eslint/no-explicit-any */
type CBORDecoded = Map<number | string, any> | any[];

type BufferLike =
  | string
  | Buffer
  | ArrayBuffer
  | ArrayBufferView
  | DataView
  | stream.Readable;

declare module "cbor-web" {
  export function encode(input: any): Buffer;
  export function decode(input: BufferLike): CBORDecoded;

  export function encodeAsync(input: any): Promise<Buffer>;
  export function decodeAsync(
    input: BufferLike,
  ): Promise<CBORDecoded>;
}
