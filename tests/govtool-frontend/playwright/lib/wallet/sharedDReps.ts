import { uploadMetadataAndGetJsonHash } from "@helpers/metadata";
import * as fs from "fs";
import path = require("path");
import {
  ensureStakeRegistered,
  isDRepRegistered,
  registeredDRepWallet,
} from "./transactions";
import { runId, testWallet, TestWallet, withFileLock } from "./testWallets";

/**
 * DReps that other tests delegate to or look up by name: dRep01 and dRep02.
 * Each is registered once per run with CIP-119 metadata and a registered
 * stake key; the given name in that metadata is recorded here so tests can
 * search the DRep directory for it.
 */
export type SharedDRepName = "dRep01" | "dRep02";

const GIVEN_NAMES_FILE = path.resolve(__dirname, "../_mock/sharedDReps.json");
const ROOT = path.resolve(__dirname, "../..");

// One entry per run id, so suites running in parallel with different
// HD_RUN_IDs do not overwrite each other's names. The older single-run shape
// ({ runId, givenNames }) is still read.
type GivenNamesFile = { runs: Record<string, Record<string, string>> };

function readFile(): GivenNamesFile {
  if (!fs.existsSync(GIVEN_NAMES_FILE)) return { runs: {} };
  const file = JSON.parse(fs.readFileSync(GIVEN_NAMES_FILE, "utf-8"));
  if (file && typeof file.runs === "object") return file as GivenNamesFile;
  if (file && typeof file.runId === "string") {
    return { runs: { [file.runId]: file.givenNames ?? {} } };
  }
  return { runs: {} };
}

function readGivenNames(): Record<string, string> {
  return readFile().runs[runId()] ?? {};
}

function recordGivenName(name: string, givenName: string) {
  return withFileLock(path.join(ROOT, ".sharedDReps.lock"), async () => {
    const file = readFile();
    file.runs[runId()] = { ...file.runs[runId()], [name]: givenName };
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
      if (recorded) {
        // A no-op once done; covers a DRep registered without its stake key.
        await ensureStakeRegistered(wallet);
        return { wallet, givenName: recorded };
      }
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
