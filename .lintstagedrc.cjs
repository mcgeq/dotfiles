module.exports = {
	'*.{js,jsx,ts,tsx}': ['prettier --cache --write .', 'eslint --fix'],
	'*.{cjs,md,json,yaml,toml}': ['prettier --cache --write'],
	'*.{vue,html}': [
		'eslint --fix',
		'prettier --cache --write',
		'stylelint --cache --fix --allow-empty-input',
	],
	'*.{scss,css}': ['stylelint --cache --fix', 'prettier --cache --write'],
};
