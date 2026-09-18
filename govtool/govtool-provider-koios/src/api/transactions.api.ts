import type {
  Envelope,
  TransactionsApi,
  TransactionState,
  TxGovernanceEffect,
} from '@govtool/data-providers/chain-data';

import { assertHexText, stripByteaPrefix } from '../common/hex';
import { envelope } from '../common/meta';
import { toIsoString } from '../common/numbers';
import type { KoiosHttpClient } from '../http/client';
import { mapDelegationTarget } from '../mappers/account.mapper';
import { toVoterRef } from '../mappers/vote.mapper';
import { encodeCip129GovActionId } from '../common/ids';
import type { TxInfoRow, TxStatusRow } from '../rows';

export class KoiosTransactionsApi implements TransactionsApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * The post-submission polling path, and one of the places Koios is simply
   * better equipped than the legacy SQL: `/tx_status` gives a confirmation
   * count, and `/tx_info` with `_governance: true` returns the transaction's
   * voting procedures, proposal procedures and certificates — enough to say
   * what the transaction actually *did*, not just that it landed.
   *
   * A hash Koios has never seen returns `unknown` rather than an error: a
   * transaction submitted seconds ago is legitimately absent, and that is the
   * state a polling UI is asking about.
   */
  async get(txHash: string): Promise<Envelope<TransactionState>> {
    assertHexText(txHash);

    const status = await this.http.post<TxStatusRow>('tx_status', {
      _tx_hashes: [txHash],
    });
    const statusRow = status.rows[0];
    if (statusRow === undefined || statusRow.num_confirmations === null) {
      return envelope({ txHash, status: 'unknown' });
    }

    const info = await this.http.post<TxInfoRow>('tx_info', {
      _tx_hashes: [txHash],
      _inputs: false,
      _metadata: false,
      _assets: false,
      _withdrawals: false,
      _certs: true,
      _scripts: false,
      _bytecode: false,
      _governance: true,
    });
    const infoRow = info.rows[0];

    const state: TransactionState = {
      txHash,
      status: 'confirmed',
      confirmations: statusRow.num_confirmations,
    };
    if (infoRow !== undefined) {
      state.includedAt = {
        epoch: infoRow.epoch_no,
        time: toIsoString(infoRow.tx_timestamp),
      };
      state.effects = mapEffects(infoRow);
      state.votingProcedures = infoRow.voting_procedures ?? null;
    }
    return envelope(state);
  }
}

/**
 * Classifies a transaction's governance content.
 *
 * Votes and proposals come from the dedicated arrays; DRep registration and
 * delegation come out of `certificates`, whose `info` is an untyped object
 * that differs per certificate type — hence the field probing rather than a
 * switch on a schema.
 */
function mapEffects(row: TxInfoRow): TxGovernanceEffect[] {
  const at = { epoch: row.epoch_no, time: toIsoString(row.tx_timestamp) };
  const effects: TxGovernanceEffect[] = [];

  for (const procedure of row.voting_procedures ?? []) {
    effects.push({
      kind: 'vote',
      vote: {
        proposal: {
          id: encodeCip129GovActionId(
            procedure.proposal_tx_hash,
            procedure.proposal_index,
          ),
          txHash: procedure.proposal_tx_hash,
          index: procedure.proposal_index,
        },
        voter: toVoterRef(
          procedure.voter_role,
          procedure.voter,
          procedure.voter_hex,
        ),
        vote:
          procedure.vote === 'Yes'
            ? 'yes'
            : procedure.vote === 'No'
              ? 'no'
              : 'abstain',
        txRef: { txHash: row.tx_hash, at },
        at,
        votingPower: null,
        rationale: null,
        isCurrent: true,
      },
    });
  }

  for (const procedure of row.proposal_procedures ?? []) {
    effects.push({
      kind: 'proposal',
      proposal: {
        id: encodeCip129GovActionId(row.tx_hash, procedure.index),
        txHash: row.tx_hash,
        index: procedure.index,
      },
    });
  }

  for (const certificate of row.certificates ?? []) {
    const info = certificate.info ?? {};
    const drepId = info['drep_id'];

    if (certificate.type.startsWith('drep_') && typeof drepId === 'string') {
      const action =
        certificate.type === 'drep_registration'
          ? 'register'
          : certificate.type === 'drep_deregistration'
            ? 'retire'
            : 'update';
      const target = mapDelegationTarget(drepId);
      if (target.kind === 'drep') {
        effects.push({ kind: 'drepRegistration', drep: target.drep, action });
      }
      continue;
    }

    if (certificate.type === 'delegation_drep' && typeof drepId === 'string') {
      effects.push({
        kind: 'delegation',
        delegation: {
          target: mapDelegationTarget(drepId),
          txRef: { txHash: row.tx_hash, at },
          since: at,
        },
      });
    }
  }

  return effects;
}

/** Kept exported: certificate hashes arrive with db-sync's `\x` prefix. */
export { stripByteaPrefix };
