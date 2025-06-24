import { verifyAsync as verifyEd25519Signature } from "@noble/ed25519";
import { blake2bHex } from "blakejs";
import { canonizeJSON } from "./canonizeJSON";
import { verifyCIP8Signature } from "./cip8verification";

export const validateSignature = async ({
  signature,
  publicKey,
  algorithm,
  jsonContent,
}: {
  signature?: string; // HEX string representation
  publicKey?: string; // HEX string representation
  algorithm?: string;
  jsonContent?: Record<string, unknown>;
}): Promise<boolean> => {
  if (!signature || !publicKey || !algorithm || !jsonContent) {
    return false;
  }

  try {
    const messageHash = await generateHashMessage(jsonContent);

    switch (algorithm) {
      case "ed25519":
      case "Ed25519": {
        return verifyEd25519Signature(signature, messageHash, publicKey);
      }
      case "CIP-8":
      case "CIP-0008": {
        return verifyCIP8Signature(signature, messageHash, publicKey);
      }
      default:
        console.error("Unsupported algorithm:", algorithm);
        return false;
    }
  } catch (error: unknown) {
    const erroMessage =
      error && typeof error === "object" && "message" in error
        ? (error as { message: string }).message
        : String(error);
    console.error("Error validating signature:", erroMessage);
    return false;
  }
};

const generateHashMessage = async (
  jsonContent: Record<string, unknown>,
): Promise<string> => {
  const { body } = jsonContent;
  const context = jsonContent["@context"];
  if (!body || !context) {
    throw new Error("Missing body or @context in jsonContent");
  }

  const jsonldDoc = {
    "@context": context,
    body,
  };

  return blake2bHex(await canonizeJSON(jsonldDoc), undefined, 32);
};
