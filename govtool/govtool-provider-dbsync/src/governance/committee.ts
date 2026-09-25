import type { CommitteeApi } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { notFound } from '../errors';
import { decodeCommitteeColdId, encodeCommitteeColdId } from '../ids';
import { readConstitution } from './committee/constitution';
import { readCommittee } from './committee/membership';

/**
 * `/governance/committee`: membership (SPEC.md §5.5, D68, D71) and the
 * constitution (D61, D71). Human identity of members is not chain data; it is
 * the optional committee info provider's (SPEC.md §8).
 *
 * Every method is async, so a bad identifier becomes a rejected promise and
 * never a synchronous throw.
 */
export function createCommitteeApi(ctx: Ctx): CommitteeApi {
  return {
    getCommittee: async () => ctx.envelope(await readCommittee(ctx)),

    getMember: async (coldCredential) => {
      const credential = decodeCommitteeColdId(coldCredential);
      // Canonical form, so a mixed-case input still matches.
      const id = encodeCommitteeColdId(credential.hash, credential.isScript);
      const committee = await readCommittee(ctx);
      const member = committee.members.find((m) => m.coldCredential === id);
      if (!member) throw notFound('Not a member of the current committee', { coldCredential });
      return ctx.envelope(member);
    },

    getConstitution: async () => ctx.envelope(await readConstitution(ctx)),
  };
}
