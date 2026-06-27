// @ts-check
// `@type` JSDoc annotations allow editor autocompletion and type checking
// (when paired with `@ts-check`).
// There are various equivalent ways to declare your Docusaurus config.
// See: https://docusaurus.io/docs/api/docusaurus-config

import {themes as prismThemes} from 'prism-react-renderer';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'AlphaMind AI',
  tagline: 'Your Second Brain',

  staticDirectories: ['assets'],

  favicon: 'img/favicon-128x128.ico',

  // Set the production url of your site here
  url: 'https://docs.alphamind.dev',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'alphamind-ai', // Usually your GitHub org/user name.
  projectName: 'alphamind', // Usually your repo name.

  onBrokenLinks: 'throw',
  markdown: {
    hooks: {
        onBrokenMarkdownLinks: 'warn',
    },
  },

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  // Add a widget for Chatwoot for live chat if users need help
  clientModules: [require.resolve('./src/components/ChatwootWidget.js')],

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: './sidebars.js',
          routeBasePath: '/',
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            'https://github.com/alphamind-ai/alphamind/tree/master/documentation/',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            'https://github.com/alphamind-ai/alphamind/tree/master/documentation/blog/',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
        sitemap: {
          lastmod: 'date',
          changefreq: 'weekly',
          priority: 0.5,
          filename: 'sitemap.xml',
        },
      }),
    ],
  ],
  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      image: 'img/alphamind_documentation.png',
      metadata: [
        {name: 'og:title', content: 'Docs'},
        {name: 'og:type', content: 'website'},
        {name: 'og:site_name', content: 'AlphaMind Documentation'},
        {name: 'og:description', content: 'Quickly get started with using or self-hosting AlphaMind'},
        {name: 'og:url', content: 'https://docs.alphamind.dev'},
        {name: 'keywords', content: 'alphamind, alphamind ai, chatgpt, open source ai, open source, transparent, accessible, trustworthy, hackable, index notes, rag, productivity'}
      ],
      navbar: {
        title: 'AlphaMind',
        logo: {
          alt: 'AlphaMind AI',
          src: 'img/favicon-128x128.ico',
        },
        items: [
          {
            href: 'https://github.com/alphamind-ai/alphamind',
            position: 'right',
            className: 'header-github-link',
            title: 'Codebase',
            'aria-label': 'GitHub repository',
          },
          {
            href: 'https://app.alphamind.dev',
            position: 'right',
            className: 'header-cloud-link',
            title: 'AlphaMind Cloud',
            'aria-label': 'AlphaMind Cloud',
          },
          {
            href: 'https://discord.gg/BDgyabRM6e',
            position: 'right',
            className: 'header-discord-link',
            title: 'Community',
            'aria-label': 'Discord community',
          },
          {
            href: 'https://blog.alphamind.dev',
            position: 'right',
            className: 'header-blog-link',
            title: 'Blog',
            'aria-label': 'AlphaMind Blog',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Get Started',
                to: '/',
              },
              {
                label: 'Privacy',
                to: '/privacy',
              },
              {
                label: 'Features',
                to: '/features/all-features',
              },
              {
                label: 'Client Apps',
                to: '/category/clients',
              },
              {
                label: 'Self-Host',
                to: '/get-started/setup',
              },
              {
                label: 'Contribute',
                to: '/contributing/development',
              },
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Discord',
                href: 'https://discord.gg/BDgyabRM6e',
              },
              {
                label: 'LinkedIn',
                href: 'https://www.linkedin.com/company/alphamind-ai/'
              },
              {
                label: 'Twitter',
                href: 'https://twitter.com/alphamind_ai',
              },
              {
                label: 'GitHub',
                href: 'https://github.com/alphamind-ai/alphamind/issues',
              },
              {
                label: 'Email',
                href: 'mailto:team@alphamind.dev',
              }
            ],
          },
          {
            title: 'More',
            items: [
              {
                href: 'https://blog.alphamind.dev',
                label: 'Blog',
              },
              {
                label: 'AlphaMind Cloud',
                href: 'https://app.alphamind.dev',
              },
              {
                label: 'GitHub',
                href: 'https://github.com/alphamind-ai/alphamind',
              },
              {
                label: 'AlphaMind Inc.',
                href: 'https://alphamind.dev',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} AlphaMind, Inc.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
      },
      algolia: {
        appId: "NBR0FXJNGW",
        apiKey: "8841b34192a28b2d06f04dd28d768017",
        indexName: "alphamind",
        contextualSearch: false,
      }
    }),
};

export default config;
