export default {
  "*.{js,jsx,ts,tsx,cjs}": ["eslint --fix"],
  "*.{md,json,yaml,toml}": ["eslint --fix"],
  "*.{vue,html}": [
    "eslint --fix",
    "stylelint --cache --fix --allow-empty-input",
  ],
  "*.{scss,css}": ["stylelint --cache --fix"],
};
