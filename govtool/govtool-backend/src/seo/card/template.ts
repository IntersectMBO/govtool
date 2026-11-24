/**
 * The 1200×630 card as a Satori element tree: plain objects in the shape JSX
 * would produce, so the backend needs no JSX setup. Colours are the app's
 * neutralWhite, textBlack and primaryBlue (frontend src/theme.ts).
 */

export const CARD_WIDTH = 1200;
export const CARD_HEIGHT = 630;

const WHITE = '#FFFFFF';
const TEXT_BLACK = '#242232';
const PRIMARY_BLUE = '#0033AD';

export type CardElement = {
  type: string;
  props: Record<string, unknown>;
};

const el = (
  type: string,
  style: Record<string, unknown>,
  children?: CardElement | CardElement[] | string,
  extra: Record<string, unknown> = {},
): CardElement => ({ type, props: { style, children, ...extra } });

/** Shorter titles are drawn larger; three lines at most either way. */
const titleSize = (title: string): number => {
  if (title.length <= 32) return 68;
  if (title.length <= 64) return 56;
  return 46;
};

export type CardText = { title: string; label: string };
export type CardImages = { logo: string; sphere: string };

export const cardElement = (
  { title, label }: CardText,
  images: CardImages,
): CardElement =>
  el(
    'div',
    {
      display: 'flex',
      width: CARD_WIDTH,
      height: CARD_HEIGHT,
      backgroundColor: WHITE,
      position: 'relative',
      fontFamily: 'Poppins, Noto Sans JP',
    },
    [
      el('img', { position: 'absolute', right: -40, top: 115 }, undefined, {
        src: images.sphere,
        width: 367,
        height: 400,
      }),
      el(
        'div',
        {
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'space-between',
          boxSizing: 'border-box',
          width: 820,
          height: CARD_HEIGHT,
          padding: '64px 72px',
        },
        [
          el('img', {}, undefined, {
            src: images.logo,
            width: 237,
            height: 48,
          }),
          el('div', { display: 'flex', flexDirection: 'column' }, [
            el(
              'div',
              {
                fontSize: 30,
                fontWeight: 400,
                color: PRIMARY_BLUE,
                marginBottom: 16,
              },
              label,
            ),
            el(
              'div',
              {
                display: 'block',
                lineClamp: 3,
                fontSize: titleSize(title),
                fontWeight: 700,
                lineHeight: 1.15,
                color: TEXT_BLACK,
              },
              title,
            ),
          ]),
          el(
            'div',
            { fontSize: 28, fontWeight: 700, color: TEXT_BLACK },
            'gov.tools',
          ),
        ],
      ),
    ],
  );
