import { blake2bHex } from "blakejs";
import * as cbor from "cbor-web";

export const getSurveyHash = (surveyDetails: Record<string, unknown>) => {
  const envelope = new Map<number, unknown>();
  envelope.set(17, {
    surveyDetails,
  });
  const encodedEnvelope = cbor.encode(envelope);
  return blake2bHex(encodedEnvelope, undefined, 32);
};
