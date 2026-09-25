import { dbInteger, safeDbInteger, compareIntegers } from './integer';
import { ProposalService } from '../proposal/proposal.service';
import { ProposalResponse } from '../proposal/proposal.type';

describe('database integer precision', () => {
  it.each(['9007199254740993', '45000000000000001', '-9007199254740993'])(
    'preserves %s exactly',
    (input) => {
      expect(dbInteger(input)).toBe(BigInt(input));
    },
  );
  it('preserves safe boundaries and integral PostgreSQL numeric strings', () => {
    expect(dbInteger('9007199254740991')).toBe(Number.MAX_SAFE_INTEGER);
    expect(dbInteger('-9007199254740991')).toBe(Number.MIN_SAFE_INTEGER);
    expect(dbInteger('42.000')).toBe(42);
    expect(dbInteger('9007199254740993.00')).toBe(9007199254740993n);
    expect(dbInteger(0)).toBe(0);
  });
  it.each(['', 'abc', '1.1', '1e20', Infinity, NaN, 9007199254740992])(
    'rejects invalid or already imprecise input %s',
    (input) => {
      expect(() => dbInteger(input)).toThrow();
    },
  );
  it('rejects unsafe bounded fields instead of rounding', () => {
    expect(() => safeDbInteger('9007199254740993')).toThrow();
    expect(safeDbInteger('42')).toBe(42);
  });
  it('compares mixed safe and arbitrary precision values without subtraction', () => {
    expect(compareIntegers(9007199254740993n, 9007199254740992n)).toBe(1);
    expect(compareIntegers(1, 9007199254740993n)).toBe(-1);
    expect(compareIntegers(1, 1n)).toBe(0);
  });
  it('sorts proposal totals exactly even when summing safe values exceeds the safe range', () => {
    const service = new ProposalService(null!, null!, null);
    const lower = {
      id: 'lower',
      dRepYesVotes: Number.MAX_SAFE_INTEGER,
      poolYesVotes: 1,
      ccYesVotes: 0,
    } as ProposalResponse;
    const higher = { ...lower, id: 'higher', poolYesVotes: 2 };
    expect(
      service.sortProposals([lower, higher], 'MostYesVotes').map((p) => p.id),
    ).toEqual(['higher', 'lower']);
  });
});
