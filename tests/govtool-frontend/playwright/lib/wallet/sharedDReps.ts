import { uploadMetadataAndGetJsonHash } from "@helpers/metadata";
import * as fs from "fs";
import path = require("path");
import { isDRepRegistered, registeredDRepWallet } from "./transactions";
import { runId, testWallet, TestWallet, withFileLock } from "./testWallets";

/**
 * DReps that other tests delegate to or look up by name: dRep01 and dRep02,
 * and dRep03 for the budget discussion tests. Each is registered once per run
 * with CIP-119 metadata; the given name in that metadata is recorded here so
 * tests can search the DRep directory for it.
 */
export type SharedDRepName = "dRep01" | "dRep02" | "dRep03";

const GIVEN_NAMES_FILE = path.resolve(__dirname, "../_mock/sharedDReps.json");
const ROOT = path.resolve(__dirname, "../..");

type GivenNames = { runId: string; givenNames: Record<string, string> };

function readGivenNames(): Record<string, string> {
  if (!fs.existsSync(GIVEN_NAMES_FILE)) return {};
  const file: GivenNames = JSON.parse(
    fs.readFileSync(GIVEN_NAMES_FILE, "utf-8")
  );
  return file.runId === runId() ? file.givenNames : {};
}

function recordGivenName(name: string, givenName: string) {
  return withFileLock(path.join(ROOT, ".sharedDReps.lock"), async () => {
    const file: GivenNames = {
      runId: runId(),
      givenNames: { ...readGivenNames(), [name]: givenName },
    };
    fs.writeFileSync(GIVEN_NAMES_FILE, JSON.stringify(file, null, 2));
  });
}

/**
 * The shared DRep with this name, registered with metadata if it is not yet.
 * The "dRep setup" project registers all of them up front; calling this from a
 * test is cheap once that has run.
 */
export function sharedDRep(
  name: SharedDRepName
): Promise<{ wallet: TestWallet; givenName: string }> {
  return withFileLock(path.join(ROOT, `.sharedDRep-${name}.lock`), async () => {
    const wallet = await testWallet(name);
    const recorded = readGivenNames()[name];
    if (await isDRepRegistered(wallet)) {
      if (recorded) return { wallet, givenName: recorded };
      throw new Error(
        `${name} is registered but its given name was not recorded in ${GIVEN_NAMES_FILE}`
      );
    }
    const { url, dataHash, givenName } = await uploadMetadataAndGetJsonHash();
    await registeredDRepWallet(name, { anchor: { url, dataHash } });
    await recordGivenName(name, givenName);
    return { wallet, givenName };
  });
}
