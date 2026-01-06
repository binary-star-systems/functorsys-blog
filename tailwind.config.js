/** @type {import('tailwindcss').Config} */
const round = (num) =>
  num
    .toFixed(7)
    .replace(/(\.[0-9]+?)0+$/, "$1")
    .replace(/\.0$/, "");
const rem = (px) => `${round(px / 16)}rem`;
const em = (px, base) => `${round(px / base)}em`;

/** @type {import('tailwindcss').Config} */
module.exports = {
  theme: {
    extend: {
      animation: {
        "gradient-move": "gradient-move 4s ease-in-out infinite alternate",
      },
      keyframes: {
        "gradient-move": {
          "0%, 100%": { "background-position": "10% 10%" },
          "50%": { "background-position": "90% 90%" },
        },
      },
      typography: () => ({
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
};
