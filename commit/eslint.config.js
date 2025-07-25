import globals from 'globals';
import pluginJs from '@eslint/js';
import tseslint from 'typescript-eslint';
import pluginReactConfig from 'eslint-plugin-react/configs/recommended.js';
import { fixupConfigRules } from '@eslint/compat';
import pluginPrettierRecommendedConfigs from 'eslint-plugin-prettier/recommended';

export default [
	{ languageOptions: { globals: { ...globals.browser, ...globals.es2020, ...globals.node } } },
	pluginJs.configs.recommended,
	...tseslint.configs.recommended,
	pluginPrettierRecommendedConfigs,
	{
		files: ['**/*.jsx', '**/*.tsx'],
		languageOptions: { parserOptions: { ecmaFeatures: { jsx: true } } },
	},
	...fixupConfigRules(pluginReactConfig),
];

