import globals from 'globals';
import pluginJs from '@eslint/js';
import tseslint from 'typescript-eslint';
import pluginReactConfig from 'eslint-plugin-react/configs/recommended.js';
import { fixupConfigRules } from '@eslint/compat';
import pluginPrettierRecommendedConfigs from 'eslint-plugin-prettier/recommended';
import vitest from 'eslint-plugin-vitest';

export default [
	{ languageOptions: { globals: { ...globals.browser, ...globals.es2020, ...globals.node } } },
	pluginJs.configs.recommended,
	...tseslint.configs.recommended,
	pluginPrettierRecommendedConfigs,
	{
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
		files: ['**/*.jsx', '**/*.tsx'],
		languageOptions: { parserOptions: { ecmaFeatures: { jsx: true } } },
	},
	...fixupConfigRules(pluginReactConfig),
];
