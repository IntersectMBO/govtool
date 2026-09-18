import { BlockfrostChainDataProvider } from '../src';
import { deriveKind, deriveStatus } from '../src/mappers/drep.mapper';
import {
  bfDRep,
  bfDRepMetadata,
  bfEpoch,
  DREP_HASH,
  DREP_ID,
  FakeBlockfrost,
} from './fake-http';

function provider(bf: FakeBlockfrost) {
  return new BlockfrostChainDataProvider(bf.client());
}

function drepRoutes(
  bf: FakeBlockfrost,
  overrides: Record<string, unknown> = {},
) {
  return bf
    .on(`/governance/dreps/${DREP_ID}`, bfDRep(overrides))
    .on(`/governance/dreps/${DREP_ID}/metadata`, bfDRepMetadata())
    .on('/epochs/latest', bfEpoch(656))
    .on('/epochs/507', bfEpoch(507))
    .on('/epochs/652', bfEpoch(652));
}

describe('governance.dreps.get', () => {
  it('derives both id forms from Blockfrost CIP-129 output', async () => {
    const bf = drepRoutes(new FakeBlockfrost());
    const { data } = await provider(bf).governance.dreps.get(DREP_ID);

    expect(data.id).toBe(DREP_ID);
    // the header byte is stripped off `hex` to get the bare credential
    expect(data.hash).toBe(DREP_HASH);
    expect(data.isScriptBased).toBe(false);
    expect(data.cip105Id).toMatch(/^drep1/);
    expect(data.cip105Id).not.toBe(DREP_ID);
  });

  it('fills both halves of an EpochStamp by resolving the epoch to a time', async () => {
    const bf = drepRoutes(new FakeBlockfrost());
    const { data } = await provider(bf).governance.dreps.get(DREP_ID);

    // Blockfrost gives `active_epoch` with no timestamp; /epochs/{n} has one.
    expect(data.registration.registeredAt).toEqual({
      epoch: 507,
      time: expect.stringMatching(/^\d{4}-/),
    });
    expect(bf.paths()).toContain('/epochs/507');
  });

  it('keeps the voting power a string and marks it the epoch snapshot', async () => {
    const bf = drepRoutes(new FakeBlockfrost());
    const { data } = await provider(bf).governance.dreps.get(DREP_ID);
    expect(data.votingPower).toEqual({
      amount: '27306302547785',
      basis: 'active',
    });
  });

  it('reports no deposit rather than claiming there is none on chain', async () => {
    const bf = drepRoutes(new FakeBlockfrost());
    const { data } = await provider(bf).governance.dreps.get(DREP_ID);
    // Blockfrost has no deposit field; `null` is the contract's "known absent".
    expect(data.registration.deposit).toBeNull();
  });

  it('infers kind from whether the credential has a metadata anchor', async () => {
    const withAnchor = drepRoutes(new FakeBlockfrost());
    await expect(
      provider(withAnchor).governance.dreps.get(DREP_ID),
    ).resolves.toMatchObject({ data: { kind: 'drep' } });

    const withoutAnchor = new FakeBlockfrost()
      .on(`/governance/dreps/${DREP_ID}`, bfDRep())
      .onStatus(`/governance/dreps/${DREP_ID}/metadata`, 404)
      .on('/epochs/latest', bfEpoch(656))
      .on('/epochs/507', bfEpoch(507))
      .on('/epochs/652', bfEpoch(652));
    const { data } =
      await provider(withoutAnchor).governance.dreps.get(DREP_ID);
    expect(data.kind).toBe('directVoter');
    expect(data.metadata).toBeNull();
  });

  it('parses the CIP-100 body wrapper and unwraps @value boxes', async () => {
    const bf = drepRoutes(new FakeBlockfrost());
    bf.on(
      `/governance/dreps/${DREP_ID}/metadata`,
      bfDRepMetadata({
        json_metadata: {
          '@context': { '@language': 'en' },
          body: {
            givenName: { '@value': 'Boxed Name' },
            objectives: 'Plain',
          },
        },
      }),
    );
    const { data } = await provider(bf).governance.dreps.get(DREP_ID);
    expect(data.metadata?.body).toEqual({
      givenName: 'Boxed Name',
      objectives: 'Plain',
    });
    expect(data.metadata?.status).toBe('valid');
  });

  it('accepts a raw hash and a CIP-105 id for the same credential', async () => {
    const bf = drepRoutes(new FakeBlockfrost());
    const api = provider(bf).governance.dreps;

    const fromCip129 = await api.get(DREP_ID);
    await api.get(DREP_HASH);
    await api.get(fromCip129.data.cip105Id!);

    // all three normalised to the same Blockfrost path
    expect(bf.callsTo(`/governance/dreps/${DREP_ID}`)).toHaveLength(3);
  });

  it('rejects a malformed id before any request', async () => {
    const bf = new FakeBlockfrost();
    await expect(
      provider(bf).governance.dreps.get('not-an-id'),
    ).rejects.toMatchObject({ code: 'INVALID_INPUT' });
    expect(bf.calls).toHaveLength(0);
  });

  it('reports NOT_FOUND for an unknown credential', async () => {
    const bf = new FakeBlockfrost().onStatus(
      `/governance/dreps/${DREP_ID}`,
      404,
    );
    await expect(
      provider(bf).governance.dreps.get(DREP_ID),
    ).rejects.toMatchObject({ code: 'NOT_FOUND' });
  });
});

describe('governance.dreps.list', () => {
  it('hydrates each id from the directory with two reads', async () => {
    const bf = drepRoutes(new FakeBlockfrost()).on('/governance/dreps', [
      { drep_id: DREP_ID, hex: bfDRep().hex },
    ]);
    const { data } = await provider(bf).governance.dreps.list({ limit: 1 });

    expect(data.elements).toHaveLength(1);
    // the directory is ids only, so detail + metadata are read per element
    expect(bf.callsTo(`/governance/dreps/${DREP_ID}`)).toHaveLength(1);
    expect(bf.callsTo(`/governance/dreps/${DREP_ID}/metadata`)).toHaveLength(1);
  });

  it('issues the next Blockfrost page as the cursor, and stops on a short page', async () => {
    const refs = Array.from({ length: 25 }, () => ({
      drep_id: DREP_ID,
      hex: bfDRep().hex,
    }));
    const full = drepRoutes(new FakeBlockfrost()).on('/governance/dreps', refs);
    const first = await provider(full).governance.dreps.list({ limit: 25 });
    expect(first.data.nextCursor).toBe('2');

    const short = drepRoutes(new FakeBlockfrost()).on('/governance/dreps', [
      { drep_id: DREP_ID, hex: bfDRep().hex },
    ]);
    const last = await provider(short).governance.dreps.list({ limit: 25 });
    expect(last.data.nextCursor).toBeNull();
  });

  it('refuses a sort rather than ordering a single page', async () => {
    const bf = new FakeBlockfrost();
    await expect(
      provider(bf).governance.dreps.list({ sort: 'votingPower' }),
    ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
    expect(bf.calls).toHaveLength(0);
  });

  it('filters the hydrated page but keeps the cursor on the Blockfrost page', async () => {
    const refs = Array.from({ length: 25 }, () => ({
      drep_id: DREP_ID,
      hex: bfDRep().hex,
    }));
    const bf = drepRoutes(new FakeBlockfrost(), { retired: true }).on(
      '/governance/dreps',
      refs,
    );
    const { data } = await provider(bf).governance.dreps.list({
      limit: 25,
      status: ['active'],
    });

    // nothing survives the filter, but the cursor still advances so a caller
    // following it reaches the rest of the directory
    expect(data.elements).toHaveLength(0);
    expect(data.nextCursor).toBe('2');
  });
});

describe('governance.dreps other routes', () => {
  it('lists delegators with their stake — something db-sync cannot do', async () => {
    const bf = new FakeBlockfrost().on(
      `/governance/dreps/${DREP_ID}/delegators`,
      [{ address: 'stake1uabc', amount: '430545196526' }],
    );
    const { data } = await provider(bf).governance.dreps.listDelegators(
      DREP_ID,
      {
        limit: 25,
      },
    );
    expect(data.elements[0]).toEqual({
      stakeAddress: 'stake1uabc',
      basis: 'live',
      balance: { total: '430545196526' },
      since: null,
      txRef: null,
    });
  });

  it('maps update actions onto the contract history types', async () => {
    const bf = new FakeBlockfrost().on(`/governance/dreps/${DREP_ID}/updates`, [
      { tx_hash: 'aa', cert_index: 0, action: 'registered' },
      { tx_hash: 'bb', cert_index: 1, action: 'deregistered' },
      { tx_hash: 'cc', cert_index: 0, action: 'updated' },
    ]);
    const { data } = await provider(bf).governance.dreps.listHistory(DREP_ID);
    expect(data.elements.map((e) => e.type)).toEqual([
      'registered',
      'retired',
      'updated',
    ]);
    // the endpoint dates none of them, which the contract now permits
    expect(data.elements[0]!.at).toBeUndefined();
  });

  it('refuses listVotes, because the response omits the proposal', async () => {
    const bf = new FakeBlockfrost();
    const error = (await provider(bf)
      .governance.dreps.listVotes(DREP_ID)
      .then(() => null)
      .catch((e: unknown) => e)) as {
      code?: string;
      details?: { reason?: string };
    };

    expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    expect(error.details?.reason).toMatch(/does not identify the proposal/);
    expect(bf.calls).toHaveLength(0);
  });

  it('refuses getVotingPowers with no ids, which would be the whole directory', async () => {
    const bf = new FakeBlockfrost();
    await expect(
      provider(bf).governance.dreps.getVotingPowers(),
    ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
    expect(bf.calls).toHaveLength(0);
  });
});

describe('derivations', () => {
  it('reads status from the three independent Blockfrost flags', () => {
    expect(deriveStatus(bfDRep())).toBe('active');
    expect(deriveStatus(bfDRep({ retired: true }))).toBe('retired');
    // retirement wins over an active flag
    expect(deriveStatus(bfDRep({ retired: true, active: true }))).toBe(
      'retired',
    );
    // expired reads as inactive, matching the contract's three-way split
    expect(deriveStatus(bfDRep({ active: false, expired: true }))).toBe(
      'inactive',
    );
  });

  it('infers kind from the presence of an anchor', () => {
    expect(deriveKind(null)).toBe('directVoter');
    expect(deriveKind(bfDRepMetadata())).toBe('drep');
  });
});
