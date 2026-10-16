// A minimal stand-in for PrismaService in the proposal-side service unit
// tests: each model method is a jest.fn the test programs, and
// `$transaction(fn)` runs `fn` against the same stub.

type Fn = jest.Mock;
export type ModelStub = Record<string, Fn>;
export type PrismaStub = Record<string, ModelStub> & {
  $transaction: Fn;
  $executeRaw: Fn;
  $queryRaw: Fn;
};

const METHODS = [
  'findUnique',
  'findUniqueOrThrow',
  'findFirst',
  'findMany',
  'count',
  'create',
  'update',
  'updateMany',
  'delete',
  'deleteMany',
];

export function prismaStub(models: string[]): PrismaStub {
  const stub: Record<string, unknown> = {};
  for (const m of models) stub[m] = Object.fromEntries(METHODS.map((k) => [k, jest.fn()]));
  stub.$executeRaw = jest.fn().mockResolvedValue(1);
  stub.$queryRaw = jest.fn().mockResolvedValue([{ id: 1 }]);
  stub.$transaction = jest.fn((fn: (tx: unknown) => unknown) => fn(stub));
  return stub as PrismaStub;
}

/** An AuthUser as the guard would build it. */
export function caller(id: number, dRepID: string | null = null) {
  const now = new Date('2026-09-26T00:00:00.000Z');
  const row = {
    id,
    username: `e0${String(id).padStart(56, '0')}`,
    govtoolUsername: null,
    isValidated: false,
    blocked: false,
    createdAt: now,
    updatedAt: now,
  };
  return { ...row, dRepID, row };
}
