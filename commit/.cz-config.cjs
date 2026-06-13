'use strict';
module.exports = {
	types: [
		{ value: ":sparkles: feature", name: "✨ feature     特性: 一个新的特性" },
    { value: ":bug: fix", name: "🐛 fix         修复: 修复一个Bug" },
    { value: ":memo: docs", name: "📝 docs        文档: 变更的只有文档" },
    {
      value: ":lipstick: style",
      name: "💄 style       格式: 空格, 分号等格式修复",
    },
    {
      value: ":recycle: refactor",
      name: "🛠 refactor     重构: 代码重构，注意和特性、修复区分开",
    },
    { value: ":zap: perf", name: "⚡️perf        性能: 提升性能" },
    { value: ":wrench: config", name: "🔧 config      配置: 修改配置文件" },
    {
      value: ":white_check_mark: test",
      name: "✅ test        测试: 添加一个测试",
    },
    { value: ":rewind: revert", name: "⏪ revert      回滚: 代码回退" },
    {
      value: ":construction: wip",
      name: "🚧 wip         进行: 正在进行中的工作",
    },
    {
      value: ":twisted_rightwards_arrows: merge",
      name: "🔀 merge       合并: 合并代码",
    },
    { value: ":package: build", name: "📦 build       打包: 打包发布" },
    { value: ":fire: prune", name: "🔥 prune       移除: 移除代码/文件" },
    { value: ":bookmark: release", name: "🔖 release     发布: 发布新版本" },
    {
      value: ":poop: poo",
      name: "💩 poo         一坨: 写了一些屎一样待优化的代码",
    },
    { value: ":tada: init", name: "🎉 init        初始: 初始化提交" },
    { value: ":rocket: deploy", name: "🚀 deploy      部署: 部署东西" },
    {
      value: ":lock: security",
      name: "🔒️security    安全: 解决安全或隐私问题",
    },
    {
      value: ":closed_lock_with_key secrets:",
      name: "🔐 secrets     机密: 添加或更新机密",
    },
    {
      value: ":construction_worker: cli",
      name: "👷 cli         构建: 添加或更新 CI 生成系统",
    },
    {
      value: ":arrow_up: dependencies",
      name: "⬆️ dependencies 依赖: 升级依赖",
    },
    {
      value: ":arrow_down: dependencies",
      name: "⬇️ dependencies 依赖: 降级依赖",
    },
    {
      value: ":pushpin: dependencies",
      name: "📌 dependencies 依赖: 将依赖固定到特定版本",
    },
    {
      value: ":heavy_plus_sign: dependencies",
      name: "➕ dependencies 依赖: 添加一个依赖",
    },
    {
      value: ":heavy_minus_sign: dependencies",
      name: "➖ dependencies 依赖: 移除一个依赖",
    },
    {
      value: ":hammer: script",
      name: "🔨 script      脚本: 添加或更新开发脚本",
    },
    {
      value: ":globe_with_meridians: globe",
      name: "🌐 globe       国际: 国际化和本地化",
    },
    { value: ":pencil2: typos", name: "✏️ typos       字体: 修正错别字" },
    {
      value: ":alien: code",
      name: "👽️alien       更新: 由于外部API更改而更新代码",
    },
    {
      value: ":truck: rename",
      name: "🚚 rename      命名: 移动或重命名资源(例如: 文件、路径、路由)",
    },
    {
      value: ":page_facing_up: license",
      name: "📄 license     协议: 添加或更新协议",
    },
    {
      value: ":bento: assets",
      name: "🍱 assets      资源: 添加或更新资源",
    },
    {
      value: ":iphone: design",
      name: "📱 design      设计: 致力于响应式设计",
    },
    {
      value: ":camera_flash: snapshots",
      name: "📸 snapshots   快照: 添加或更新快照",
    },
    {
      value: ":coffin: code",
      name: "⚰️ code        代码: 删除死代码",
    },
    {
      value: ":clown_face: mock",
      name: "🤡 mock        模拟: Mock things",
    },
	
	],

	scopes: ['config', 'frontend', 'backend', 'build', 'docs', 'style', 'cli'],
	allowCustomScopes: true,
	allowBreakingChanges: ['feat', 'fix'],

	allowTicketNumber: false,
	isTicketNumberRequired: false,
	ticketNumberPrefix: 'RICKET-',
	ticketNumberRegExp: '\\d{1,5}',

	skipEmptyScopes: true,
	messages: {
		type: '选择一种你的提交类型:',
		scope: '选择影响的范围(可选):',
		customScope: '请输入本次提交涉及的模块或范围(可选):',
		subject: '本次提交说明(短说明):',
		body: '本次提交说明(长说明)，使用"|"换行(可选):',
		breaking: '非兼容性说明 (可选):',
		footer: '关联关闭的issue，例如：#31, #34(可选):',
		confirmCommit: '确定提交说明?(y/n)',
	},
	// 自定义填充信息
	upperCaseSubject: true,
	subjectLimit: 100,
	breaklineChar: '|',
};
