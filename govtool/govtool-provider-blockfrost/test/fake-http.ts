import { BlockfrostClient } from '../src/http/client';

export interface RecordedCall {
  path: string;
  search: Record<string, string>;
}

export interface Route {
  status?: number;
  body?: unknown;
  /** Bodies returned in order, for testing pagination. */
  bodies?: unknown[];
  throws?: Error;
}

/**
 * A `BlockfrostClient` wired to a routing table instead of the network, so a
 * test asserts the exact paths and query parameters the provider requests —
 * which is the part that must not drift — without touching Blockfrost.
 */
export class FakeBlockfrost {
  readonly calls: RecordedCall[] = [];
  private readonly routes = new Map<string, Route>();
  private readonly cursors = new Map<string, number>();

  on(path: string, body: unknown, status = 200): this {
    this.routes.set(path, { body, status });
    return this;
  }

  onPages(path: string, bodies: unknown[]): this {
    this.routes.set(path, { bodies, status: 200 });
    return this;
  }

  onStatus(path: string, status: number, body: unknown = null): this {
    this.routes.set(path, { status, body });
    return this;
  }

  onThrow(path: string, error: Error): this {
    this.routes.set(path, { throws: error });
    return this;
  }

  callsTo(path: string): RecordedCall[] {
    return this.calls.filter((c) => c.path === path);
  }

  /** Every path requested, in order — useful for asserting fan-out. */
  paths(): string[] {
    return this.calls.map((c) => c.path);
  }

  client(
    overrides: Partial<ConstructorParameters<typeof BlockfrostClient>[0]> = {},
  ) {
    const fetchImpl: typeof fetch = (input) => {
      const url = new URL(String(input));
      const search: Record<string, string> = {};
      url.searchParams.forEach((v, k) => (search[k] = v));
      this.calls.push({ path: url.pathname, search });

      const route = this.routes.get(url.pathname);
      if (route === undefined) {
        return Promise.resolve(
          new Response(JSON.stringify({ error: 'Not Found' }), { status: 404 }),
        );
      }
      if (route.throws) return Promise.reject(route.throws);

      let body = route.body;
      if (route.bodies !== undefined) {
        const seen = this.cursors.get(url.pathname) ?? 0;
        body = route.bodies[Math.min(seen, route.bodies.length - 1)];
        this.cursors.set(url.pathname, seen + 1);
      }
      return Promise.resolve(
        new Response(body === null ? '' : JSON.stringify(body), {
          status: route.status ?? 200,
        }),
      );
    };

    return new BlockfrostClient({
      baseUrl: 'https://bf.test',
      fetch: fetchImpl,
      // No real waiting in tests.
      sleep: () => Promise.resolve(),
      maxRetries: 2,
      ...overrides,
    });
  }
}

/* ------------------------------------------------------------------------- */
/* Fixtures, shaped exactly as blockfrost-ryo 3.1.1 returns them             */
/* ------------------------------------------------------------------------- */

export const DREP_ID =
  'drep1yfaaaaa270yjt6tu5skndugekprf5ykv5jshanl0c6gqx5qpstskf';
/** The same credential's CIP-129 hex: 0x22 header + 28-byte hash. */
export const DREP_HEX =
  '227bdef7aaf3c925e97ca42d36f119b0469a12cca4a17ecfefc6900350';
export const DREP_HASH =
  '7bdef7aaf3c925e97ca42d36f119b0469a12cca4a17ecfefc6900350';

export function bfDRep(overrides: Record<string, unknown> = {}) {
  return {
    drep_id: DREP_ID,
    hex: DREP_HEX,
    amount: '27306302547785',
    active: true,
    active_epoch: 507,
    has_script: false,
    retired: false,
    expired: false,
    last_active_epoch: 652,
    ...overrides,
  };
}

export function bfDRepMetadata(overrides: Record<string, unknown> = {}) {
  return {
    drep_id: DREP_ID,
    hex: DREP_HEX,
    url: 'https://drep.one/drep.jsonld',
    hash: '594631799f4b1c17379893a9a56cf46a0cd4c96113867c8086aaacb66775dd2d',
    json_metadata: {
      body: { givenName: 'Drep One', objectives: 'Vote on everything' },
    },
    ...overrides,
  };
}

export const PROPOSAL_TX =
  '15f82a365bdee483a4b03873a40d3829cc88c048ff3703e11bd01dd9e035c916';

export function bfProposal(overrides: Record<string, unknown> = {}) {
  return {
    tx_hash: PROPOSAL_TX,
    cert_index: 0,
    governance_type: 'info_action',
    governance_description: { tag: 'InfoAction' },
    deposit: '100000000000',
    return_address:
      'stake1u9zaaehwt4lkxxmzym2972w6gywy97n7s9kup9ydx8sdhfcjxtusp',
    ratified_epoch: null,
    enacted_epoch: null,
    dropped_epoch: null,
    expired_epoch: null,
    expiration: 514,
    ...overrides,
  };
}

export function bfEpoch(
  epoch: number,
  overrides: Record<string, unknown> = {},
) {
  return {
    epoch,
    // 2026-09-16T21:44:51Z for epoch 656, one epoch = 432000s
    start_time: 1789595091 - (656 - epoch) * 432000,
    end_time: 1790027091 - (656 - epoch) * 432000,
    first_block_time: 1789595152,
    last_block_time: 1789717340,
    block_count: 6096,
    tx_count: 30854,
    output: '9187956522931092',
    fees: '9279359547',
    active_stake: '21370797071255004',
    ...overrides,
  };
}

export function bfBlock(overrides: Record<string, unknown> = {}) {
  return {
    time: 1789717340,
    height: 13955938,
    hash: '1051b1054d3a5094278b610dc8091226720f1b386728ea02812cdeba9b62215f',
    slot: 198151049,
    epoch: 656,
    epoch_slot: 122249,
    tx_count: 12,
    ...overrides,
  };
}

export const GENESIS = {
  active_slots_coefficient: 0.05,
  update_quorum: 5,
  max_lovelace_supply: '45000000000000000',
  network_magic: 764824073,
  epoch_length: 432000,
  system_start: 1506203091,
  slots_per_kes_period: 129600,
  slot_length: 1,
  max_kes_evolutions: 62,
  security_param: 2160,
};
