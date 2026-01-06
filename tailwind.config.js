/** @type {import('tailwindcss').Config} */
const round = (num) =>
  num
    .toFixed(7)
    .replace(/(\.[0-9]+?)0+$/, '$1')
    .replace(/\.0$/, '')
const rem = (px) => `${round(px / 16)}rem`
const em = (px, base) => `${round(px / base)}em`

/** @type {import('tailwindcss').Config} */
module.exports = {
  theme: {
    extend: {
      animation: {
        'gradient-move': 'gradient-move 4s ease-in-out infinite alternate',
      },
      keyframes: {
        'gradient-move': {
          '0%, 100%': { 'background-position': '10% 10%' },
          '50%': { 'background-position': '90% 90%' },
        },
      },
      typography: () => ({
        DEFAULT: {
          css: {
            h1: {
              fontFamily: 'var(--font-sans)',
              fontSize: rem(20),
              marginTop: em(16, 20),
              marginBottom: em(4, 20),
            },
            h2: {
              fontFamily: 'var(--font-sans)',
              fontSize: rem(18),
              marginTop: em(14, 18),
              marginBottom: em(4, 18),
            },
            h3: {
              fontFamily: 'var(--font-sans)',
              fontSize: rem(16),
              marginTop: em(12, 16),
              marginBottom: em(2, 16),
            },
            h4: {
              fontFamily: 'var(--font-sans)',
              fontSize: rem(15),
              marginTop: em(10, 15),
              marginBottom: em(2, 15),
            },
            p: {
              marginBottom: em(20, 16),
            },
            blockquote: {
              borderLeftWidth: '3px',
              borderLeftColor: 'rgb(var(--divider-color))',
              paddingLeft: em(20, 16),
              fontStyle: 'italic',
              color: 'rgb(var(--muted-color))',
            },
          },
        },
        lg: {
          css: [
            {
              lineHeight: 1.6,
              fontSize: rem(17.75),
            },
          ],
        },
        xl: {
          css: [
            {
              lineHeight: 1.6,
            },
          ],
        },
      }),
    },
  },
}
