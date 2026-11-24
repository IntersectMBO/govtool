import { ServiceUnavailableException } from '@nestjs/common';

import {
  CardService,
  cardDigest,
  cardHash,
  scriptSupported,
} from '../src/seo/card/card.service';
import type { CardInput } from '../src/seo/pages';

jest.setTimeout(60_000);

const PNG_SIGNATURE = Buffer.from([
  0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,
]);

const card = (overrides: Partial<CardInput> = {}): CardInput => ({
  kind: 'action',
  id: 'gov_action1example',
  title: 'State of the Cardano Developer Ecosystem: 2026 Survey',
  label: 'Info action · Voting open',
  ...overrides,
});

describe('cardHash', () => {
  it('is 12 hex characters', () => {
    expect(cardHash(card())).toMatch(/^[0-9a-f]{12}$/);
  });

  it('changes when only the label changes', () => {
    expect(cardHash(card())).not.toBe(
      cardHash(card({ label: 'Info action · Ratified in epoch 580' })),
    );
  });

  it('ignores emoji and markup that are never drawn', () => {
    expect(cardHash(card({ title: '**Fund** 🚀 it' }))).toBe(
      cardHash(card({ title: 'Fund it' })),
    );
  });
});

describe('scriptSupported', () => {
  it.each([
    ['Fund the tooling: Ñandú café', true],
    ['日本コミュニティとガバナンス', true],
    ['Привет, мир', true],
    ['Ελλας', true],
    // Not in the bundled fonts' character maps: drawn as the default card.
    ['Ελλάδα', false],
    ['Україна', false],
    ['我们开会', false],
    ['안녕하세요', false],
    ['مرحبا', false],
  ])('%s → %p', (text, supported) => {
    expect(scriptSupported(text)).toBe(supported);
  });
});

describe('CardService', () => {
  const service = new CardService();

  it('renders a 1200×630 PNG under 300 KB', async () => {
    const png = await service.render(card());
    expect(png.subarray(0, 8)).toEqual(PNG_SIGNATURE);
    expect(png.readUInt32BE(16)).toBe(1200);
    expect(png.readUInt32BE(20)).toBe(630);
    expect(png.length).toBeLessThan(300_000);
  });

  it('draws Japanese text with real glyphs', async () => {
    const japanese = await service.renderSvg(
      card({ title: '日本コミュニティとガバナンス' }),
    );
    const blank = await service.renderSvg(card({ title: '  ' }));
    expect(japanese.length).toBeGreaterThan(blank.length + 2_000);
  });

  it('draws the default card for a script it has no font for', async () => {
    const korean = await service.render(card({ title: '안녕하세요' }));
    expect(korean.equals(await service.renderDefault())).toBe(true);
  });

  it.each(['Ελλάδα', 'Україна', '我们开会'])(
    'draws the default card for %s, which the fonts lack',
    async (title) => {
      const png = await service.render(card({ title }));
      expect(png.equals(await service.renderDefault())).toBe(true);
    },
  );

  it('hands Satori every font up front, with no per-render font loading', () => {
    const options = new CardService().satoriOptions();
    expect('loadAdditionalAsset' in options).toBe(false);
    expect(new Set(options.fonts.map((f) => f.name))).toEqual(
      new Set(['Poppins', 'Noto Sans JP']),
    );
  });

  it('renders a card once however many ask for it at the same time', async () => {
    const fresh = new CardService();
    const spy = jest.spyOn(fresh, 'renderSvg');
    const pngs = await Promise.all(
      Array.from({ length: 10 }, () =>
        fresh.render(card({ title: 'A viral share' })),
      ),
    );
    expect(spy).toHaveBeenCalledTimes(1);
    expect(new Set(pngs).size).toBe(1);
  });

  it('renders off the main thread with resvg renderAsync', async () => {
    // The module object card.service requires, not an import-star copy.
    const resvg =
      jest.requireActual<typeof import('@resvg/resvg-js')>('@resvg/resvg-js');
    const spy = jest.spyOn(resvg, 'renderAsync');
    await new CardService().render(card({ title: 'Off the main thread' }));
    expect(spy).toHaveBeenCalledTimes(1);
    spy.mockRestore();
  });

  it('keys rendered cards by the full digest, not the 12-character URL hash', async () => {
    const fresh = new CardService();
    const c = card({ title: 'Keyed by digest' });
    await fresh.render(c);
    const keys = [...(fresh['cards'] as Map<string, unknown>).keys()];
    expect(cardDigest(c)).toMatch(/^[0-9a-f]{64}$/);
    expect(keys).toEqual([cardDigest(c)]);
    expect(cardHash(c)).toBe(cardDigest(c).slice(0, 12));
  });

  it('answers 503 to a render that waits too long for a slot', async () => {
    const busy = new CardService({ maxConcurrent: 1, waitMs: 50 });
    jest
      .spyOn(busy, 'renderSvg')
      .mockImplementation(
        () =>
          new Promise((resolve) => setTimeout(() => resolve('<svg/>'), 300)),
      );
    const first = busy.render(card({ title: 'first' }));
    const second = busy.render(card({ title: 'second' }));
    await expect(second).rejects.toBeInstanceOf(ServiceUnavailableException);
    await first.catch(() => undefined);
  });
});
