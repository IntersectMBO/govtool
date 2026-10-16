import { describe, expect, it } from 'vitest';
import { render } from '@testing-library/react';
import MarkdownTypography from '../markdownRenderer';

const renderMarkdown = (content) =>
    render(<MarkdownTypography content={content} testId='markdown' />)
        .container;

describe('MarkdownTypography', () => {
    it('drops iframes and scripts from raw HTML', () => {
        const container = renderMarkdown(
            [
                'Before',
                '<iframe src="https://example.com"></iframe>',
                '<iframe srcdoc="<script>alert(1)</script>"></iframe>',
                '<script>alert(1)</script>',
                'After',
            ].join('\n\n')
        );

        expect(container.querySelector('iframe')).toBeNull();
        expect(container.querySelector('script')).toBeNull();
        expect(container.textContent).toContain('Before');
        expect(container.textContent).toContain('After');
    });

    it('drops style tags, inline styles and unsafe attributes', () => {
        const container = renderMarkdown(
            [
                '<style>.injected { display: none; }</style>',
                '<div style="position:fixed;inset:0">overlay</div>',
                '<img src="https://example.com/a.png" onerror="alert(1)">',
                '<a href="javascript:alert(1)">link</a>',
            ].join('\n\n')
        );

        const injectedStyle = [...document.querySelectorAll('style')].some(
            (node) => node.textContent.includes('.injected')
        );
        expect(injectedStyle).toBe(false);
        expect(container.querySelector('div[style*="fixed"]')).toBeNull();
        expect(container).toHaveTextContent('overlay');
        expect(container.querySelector('img[onerror]')).toBeNull();
        expect(container.querySelector('a[href^="javascript"]')).toBeNull();
    });

    it('keeps the formatting tags added by preprocessing', () => {
        const container = renderMarkdown(
            ['==marked==', '++inserted++', '19^th^', 'H~2~O', '~~gone~~'].join(
                '\n\n'
            )
        );

        expect(container.querySelector('mark')).toHaveTextContent('marked');
        expect(container.querySelector('ins')).toHaveTextContent('inserted');
        expect(container.querySelector('sup')).toHaveTextContent('th');
        expect(container.querySelector('sub')).toHaveTextContent('2');
        expect(container.querySelector('del')).toHaveTextContent('gone');
    });

    it('still renders math and code blocks', () => {
        const container = renderMarkdown(
            [
                'Inline $\\frac{a}{b}$',
                '$$\n\\sqrt{x}\n$$',
                '```js\nconst a = 1;\n```',
            ].join('\n\n')
        );

        const annotations = [
            ...container.querySelectorAll('math annotation'),
        ].map((node) => node.textContent);
        expect(annotations).toEqual(['\\frac{a}{b}', '\\sqrt{x}']);
        expect(container.querySelector('math[display="block"]')).not.toBeNull();
        expect(container.querySelector('code.language-js')).not.toBeNull();
    });
});
