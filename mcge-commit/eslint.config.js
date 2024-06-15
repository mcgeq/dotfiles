import globals from 'globals';
import pluginJs from '@eslint/js';
import tseslint from 'typescript-eslint';
import reactRecommended from 'eslint-plugin-react/configs/recommended.js';
import react from 'eslint-plugin-react/configs/all.js';
import pluginPrettierRecommendedConfigs from 'eslint-plugin-prettier/recommended';
import vitest from 'eslint-plugin-vitest';

export default [
	pluginJs.configs.recommended,
	...tseslint.configs.recommended,
	pluginPrettierRecommendedConfigs,
	{
		languageOptions: {
			globals: {
				...globals.browser,
				...globals.es2020,
				...globals.node,
			},
		},
	},
	{
		name: 'vitest/recommended',
		files: ['test/**'],
		plugins: {
			vitest,
		},
		rules: {
			...vitest.configs.recommended.rules,
			'vitest/max-nested-describe': ['error', { max: 3 }],
		},
		settings: {
			vitest: {
				typecheck: true,
			},
		},
		languageOptions: {
			globals: {
				...vitest.environments.env.globals,
			},
		},
	},
	{
		files: ['**/*.{js,ts,mjs,cjs,jsx,mjsx,tsx,mtsx}'],
		plugins: {
			react,
		},
		settings: {
			react: {
				version: 'detect',
			},
		},
		...reactRecommended,
		languageOptions: {
			...reactRecommended.languageOptions,
			globals: {
				...globals.serviceworker,
			},
		},
	},
];
