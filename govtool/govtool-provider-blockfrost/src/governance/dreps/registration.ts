/**
 * A DRep's registration facts from `/governance/dreps/{id}/updates`.
 *
 * The certificate feed says which transaction and which certificate, and the
 * deposit, but not when: each certificate is dated by a `/txs/{hash}` read.
 * That is the per-DRep cost of the required `registration` field, and why a
 * directory page costs about two requests per DRep on it.
 *
 *   latest        newest `registered` certificate
 *   latestUpdate  newest `updated` certificate after it, or null
 *   retiredAt     not served: it would be a third dated read per retired DRep
 */
import type { Registration, RegistrationEvent } from '@govtool/data-providers/chain-data';

import { txStamp } from '../../chain';
import type { Session } from '../../context';
import { internal } from '../../errors';
import { toLovelace } from '../../numbers';

export interface BfDRepUpdate {
  tx_hash: string;
  cert_index: number;
  action: 'registered' | 'updated' | 'deregistered' | string;
  deposit: string | null;
}

/** Every certificate, oldest first. */
export const loadUpdates = (s: Session, id: string): Promise<BfDRepUpdate[]> =>
  s.once(`updates:${id}`, () => s.http.getAll<BfDRepUpdate>(`/governance/dreps/${id}/updates`, { batch: 1 }));

export interface CertIndex {
  firstRegistration: BfDRepUpdate;
  latestRegistration: BfDRepUpdate;
  latestUpdate: BfDRepUpdate | null;
  /** The newest deregistration, while it is the newest registration/deregistration. */
  retirement: BfDRepUpdate | null;
}

export function indexCertificates(updates: readonly BfDRepUpdate[], id: string): CertIndex {
  let first: BfDRepUpdate | undefined;
  let latest: BfDRepUpdate | undefined;
  let update: BfDRepUpdate | null = null;
  let retirement: BfDRepUpdate | null = null;
  for (const u of updates) {
    if (u.action === 'registered') {
      first ??= u;
      latest = u;
      update = null;
      retirement = null;
    } else if (u.action === 'updated') {
      if (latest) update = u;
    } else if (u.action === 'deregistered') {
      retirement = u;
    } else {
      throw internal(`Blockfrost reported an unknown DRep certificate action '${u.action}'`, { id });
    }
  }
  if (!first || !latest) throw internal('Blockfrost lists a DRep with no registration certificate', { id });
  return { firstRegistration: first, latestRegistration: latest, latestUpdate: update, retirement };
}

async function event(s: Session, cert: BfDRepUpdate): Promise<RegistrationEvent> {
  const at = await txStamp(s, cert.tx_hash);
  return {
    txRef: { txHash: cert.tx_hash, index: cert.cert_index, ...(at.block === undefined ? {} : { block: at.block }) },
    at,
    // Only a registration carries a deposit; an update has none.
    deposit: cert.deposit === null ? null : toLovelace(cert.deposit, 'DRep deposit'),
  };
}

export async function loadRegistration(s: Session, id: string): Promise<Registration> {
  const certs = indexCertificates(await loadUpdates(s, id), id);
  const [latest, latestUpdate] = await Promise.all([
    event(s, certs.latestRegistration),
    certs.latestUpdate ? event(s, certs.latestUpdate) : Promise.resolve(null),
  ]);
  return { latest, latestUpdate };
}
