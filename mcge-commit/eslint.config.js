import globals from 'globals';
import jseslint from '@eslint/js';
import tseslint from 'typescript-eslint';
import reactRecommended from 'eslint-plugin-react/configs/recommended.js';
import react from 'eslint-plugin-react/configs/all.js';
import pluginPrettierRecommendedConfigs from 'eslint-plugin-prettier/recommended';
import vitest from 'eslint-plugin-vitest';
import eslint_vue from 'eslint-plugin-vue';
import vue_parser from 'vue-eslint-parser';

export default [
	jseslint.configs.recommended,
	...tseslint.configs.recommended,
	pluginPrettierRecommendedConfigs,
	...eslint_vue.configs['flat/recommended'],
	{
		languageOptions: {
			parserOptions: {
				sourceType: 'module',
				parser: {
					ts: tseslint.parser,
				},
			},
			globals: {
				...globals.browser,
				...globals.es2020,
				...globals.node,
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
	{
		name: 'vue/parser',
		files: ['**/*.vue'],
		languageOptions: {
			parser: vue_parser,
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
];
