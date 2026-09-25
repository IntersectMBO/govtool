import * as fs from "fs";
import { HD_RUN_FILE } from "./testWallets";

/**
 * Global setup: picks this run's HD account base, so the run's wallets have no
 * chain history. Hardened account indices stop at 2^31; roles use the next
 * few indices above the base. HD_ACCOUNT_BASE pins it, e.g. to reuse a run's
 * wallets.
 */
export default async function hdRunSetup() {
  const base = process.env.HD_ACCOUNT_BASE
    ? Number(process.env.HD_ACCOUNT_BASE)
    : (Math.floor(Math.random() * 2 ** 20) + 1) * 1024;
  fs.writeFileSync(
    HD_RUN_FILE,
    JSON.stringify({ base, startedAt: new Date().toISOString() }, null, 2)
  );
}
