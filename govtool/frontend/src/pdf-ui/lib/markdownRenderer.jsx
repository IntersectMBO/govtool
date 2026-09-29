import React, { useEffect } from 'react';
import ReactMarkdown from 'react-markdown';
import remarkBreaks from 'remark-breaks';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import remarkGfm from 'remark-gfm';
import rehypeRaw from 'rehype-raw';
import { Box } from '@mui/material';
import { Typography } from '@atoms';
// GovTool's markdown table styles (striped rows, #D6E2FF borders), scoped
// to `.markdown`, as in GovernanceActionCardElement.
import '@/components/molecules/tableMarkdown.css';
import 'highlight.js/styles/github.css';
import hljs from 'highlight.js';
import { useTheme } from '@emotion/react';
import { fadedPurple, primaryBlue } from '@/consts/colors';

// List of Markdown tags that we want to consider for format
const markdownTags = [
    'p',
    'h1',
    'h2',
    'h3',
    'h4',
    'h5',
    'h6',
    'li',
    'blockquote',
    'em',
    'strong',
    'del',
    'span',
    'hr',
    'a',
    'table',
    'thead',
    'tbody',
    'tr',
    'th',
    'td',
];

// Map Markdown tags to GovTool `Typography` variants. The element itself is
// set through `component={tag}`, since the atom does not forward `variant`.
const typographyVariants = {
    p: 'body1',
    h1: 'headline4',
    h2: 'title1',
    h3: 'title2',
    h4: 'body1',
    h5: 'body2',
    h6: 'caption',
    li: 'body1',
    blockquote: 'body2',
    em: 'body1',
    strong: 'body1',
    del: 'body1',
    span: 'body1',
    hr: 'body1',
};

// Body text stays regular weight (the atom's body1/body2 are 600/500).
const fontWeights = {
    h1: 600,
    h2: 400,
    h3: 600,
    strong: 700,
};

const fontSizes = {
    h3: 18,
};

const fontWeightFor = (tag) => fontWeights[tag] ?? 400;

const symbolReplacements = (text) =>
    text
        ?.replace(/\(c\)/gi, '©')
        ?.replace(/\(r\)/gi, '®')
        ?.replace(/\(tm\)/gi, '™')
        ?.replace(/\(p\)/gi, '℗')
        ?.replace(/\+\/-/g, '±')
        ?.replace(/\.\.+/g, '…') // change .. with ...
        ?.replace(/([?!])…/g, '$1..') // Change?... with ?..
        ?.replace(/([?!]){4,}/g, '$1$1$1') // Search multiply ? or !
        ?.replace(/,{2,}/g, ',') // Change multiple commas with one
        ?.replace(/(^|\s)--(?=\s|$)/gm, '$1\u2013') // Change -- with en-dash
        ?.replace(/(^|[^-\s])--(?=[^-\s]|$)/gm, '$1\u2013'); // Change -- with en-dash in context
// .replace(/(^|[^-])---(?=[^-]|$)/gm, '$1\u2014') // Change --- with em-dash

const MarkdownTypography = ({ content, testId, onLinkClick }) => {
    const theme = useTheme();
    const typographyComponents = markdownTags.reduce((acc, tag) => {
        acc[tag] = ({ href, children, style, align }) => {
            // Render hr tag without any modification then this
            if (tag === 'hr') {
                return <hr style={{ marginTop: 0, marginBottom: '1rem' }} />;
            }

            if (tag === 'ul') {
                return (
                    <ul
                        style={{
                            display: 'block',
                            'list-style-type': 'disc',
                            'margin-block-start': '1em',
                            'margin-block-end': '1em',
                            'margin-inline-start': '0px',
                            'margin-inline-end': '0px',
                            'padding-inline-start': '40px',
                            'unicode-bidi': 'isolate',
                        }}
                    >
                        {children}
                    </ul>
                );
            }

            if (tag === 'li') {
                return (
                    <li>
                        <Typography
                            variant='body1'
                            component='span'
                            fontWeight={400}
                            style={{
                                wordWrap: 'break-word',
                                marginTop: 0,
                                marginBottom: '1rem',
                            }}
                        >
                            {children}
                        </Typography>
                    </li>
                );
            }

            if (tag === 'blockquote') {
                return (
                    <Typography
                        variant={typographyVariants[tag] || 'body1'}
                        component={tag}
                        fontWeight={fontWeightFor(tag)}
                        style={{
                            borderLeft: `4px solid ${theme.palette.lightBlue}`,
                            paddingLeft: '1em',
                            color: theme.palette.neutralGray,
                            margin: '0.5em 0',
                            backgroundColor: 'transparent',
                            marginTop: 0,
                            marginBottom: '1rem',
                        }}
                    >
                        {children}
                    </Typography>
                );
            }

            if (tag === 'a') {
                return (
                    <a
                        href={href}
                        style={{
                            color: primaryBlue.c500,
                            wordBreak: 'break-word',
                        }}
                        onClick={(e) => {
                            if (onLinkClick) {
                                onLinkClick(href, e);
                            }
                        }}
                    >
                        {children}
                    </a>
                );
            }

            if (tag === 'table') {
                return (
                    <table
                        style={{
                            display: 'block',
                            overflowX: 'auto',
                            margin: '32px 0',
                            borderSpacing: 0,
                            borderCollapse: 'collapse',
                            maxWidth: '100%',
                        }}
                    >
                        {children}
                    </table>
                );
            }
            if (tag === 'thead') {
                return (
                    <thead
                        style={{
                            backgroundColor: `${primaryBlue.c100}80`,
                        }}
                    >
                        {children}
                    </thead>
                );
            }
            if (tag === 'tbody') {
                return <tbody>{children}</tbody>;
            }
            if (tag === 'tr') {
                return (
                    <tr
                        style={{
                            ':nth-child(2n)': {
                                backgroundColor: `${primaryBlue.c100}80`,
                            },
                        }}
                    >
                        {children}
                    </tr>
                );
            }
            if (tag === 'th') {
                return (
                    <th
                        style={{
                            padding: '6px 13px',
                            border: `1px solid ${theme.palette.lightBlue}`,
                        }}
                    >
                        {children}
                    </th>
                );
            }
            if (tag === 'td') {
                return (
                    <td
                        style={{
                            padding: '6px 13px',
                            border: `1px solid ${theme.palette.lightBlue}`,
                        }}
                    >
                        <div style={{ marginBottom: 0 }}>{children}</div>
                    </td>
                );
            }

            return (
                <Typography
                    variant={typographyVariants[tag] || 'body1'}
                    component={tag}
                    style={{
                        whiteSpace: 'pre-line',
                        wordWrap: 'break-word',
                        marginTop: 0,
                        marginBottom: '1rem',
                    }}
                    fontWeight={fontWeightFor(tag)}
                    {...(fontSizes[tag] && { fontSize: fontSizes[tag] })}
                >
                    {React.Children.map(children, (child) => {
                        if (child.type === 'code') {
                            return (
                                <code
                                    style={{
                                        fontFamily: 'monospace',
                                        fontSize: '0.875em',
                                        backgroundColor:
                                            fadedPurple.c50,
                                        padding: '2px 4px',
                                        borderRadius: '4px',
                                    }}
                                >
                                    {child.props.children}
                                </code>
                            );
                        }
                        return child;
                    })}
                </Typography>
            );
        };
        return acc;
    }, {});

    const preprocessContent = (text) => {
        const indentedCodeRegex = /(^|\n)( {4,}.*)/g; // recognize 4 intends
        let preservedIndentedCode = [];
        let preservedTextIndex = 0;

        // Save text with intends and its placeholders
        text = text?.replace(indentedCodeRegex, (match) => {
            preservedIndentedCode[preservedTextIndex] = match;
            preservedTextIndex++;
            return `<!--indentedCode${preservedTextIndex - 1}-->`;
        });

        const preserveCodeBlocksRegex = /```(.*?)```/gs;
        let preservedCodeBlocks = [];
        text = text?.replace(preserveCodeBlocksRegex, (match, codeContent) => {
            preservedCodeBlocks.push(codeContent);
            return `<!--codeBlock${preservedCodeBlocks.length - 1}-->`;
        });

        // // Change every \n\n or more with &nbsp;
        // text = text.replace(/\n{2,}/g, (match) => {
        //     // const newLines = match.length; // number of empty lines that needs to be added
        //     // return '\n' + '&nbsp;\n'.repeat(newLines); // Add that number of lines with &nbsp;
        //     // const count = match.length - 1;
        //     // return `\n\n{{BLANK}}\n\n`;
        // });

        preservedCodeBlocks.forEach((codeBlock, index) => {
            text = text?.replace(
                `<!--codeBlock${index}-->`,
                `\`\`\`${codeBlock}\`\`\``
            );
        });

        // Handle strikethrough text
        text = text?.replace(/~~(.*?)~~/g, '<del>$1</del>'); // This will replace ~~text~~ with <del>text</del>

        // Recognition of marked text (==text==) and change with <mark> tags
        text = text?.replace(/==(.*?)==/g, '<mark>$1</mark>'); // This will replace ==text== with <mark>text</mark>

        // Recognition of inserted text (++text++) and change with <ins> tags
        text = text?.replace(/\+\+(.*?)\+\+/g, '<ins>$1</ins>'); // This will replace ++text++ with <ins>text</ins>

        // Recognition superscript (19^th^) and change with <sup> tags
        text = text?.replace(/\^([^~\^]+)\^/g, '<sup>$1</sup>'); // This will replace 19^th^ with <sup>th</sup>

        // Recognition subscript (H~2~O) and change with <sub> tags
        text = text?.replace(/~([^~]+)~/g, '<sub>$1</sub>'); // This will replace H~2~O with <sub>2</sub>O

        // Return code and comments back with their values
        preservedIndentedCode.forEach((code, index) => {
            text = text?.replace(`<!--indentedCode${index}-->`, code);
        });

        // // Table regex
        // const tableRegex = /\|([^\n]+)\|\n\|([^\n]+)\|\n([\s\S]+?)\n/g;

        // text = text.replace(
        //     tableRegex,
        //     (match, headerRow, separatorRow, bodyRows) => {
        //         // Izdvajamo zaglavlje
        //         const headers = headerRow
        //             .split('|')
        //             .map((col) => col.trim())
        //             .filter(Boolean);

        //         // Pronađemo i zanemarimo separator (samo da proverimo da li postoji)
        //         const separators = separatorRow
        //             .split('|')
        //             .map((col) => col.trim())
        //             .filter(Boolean);

        //         // Uzimamo redove sa podacima (telo tabele)
        //         const rows = bodyRows
        //             .trim()
        //             .split('\n')
        //             .map((row) => {
        //                 return row
        //                     .split('|')
        //                     .map((col) => col.trim())
        //                     .filter(Boolean);
        //             });

        //         // Pravimo HTML tabelu
        //         const tableHTML = `
        //     <table style="width: 100%; border-collapse: collapse; border: 1px solid #ddd;">
        //         <thead>
        //             <tr>
        //                 ${headers.map((col) => `<th style="border: 1px solid #ddd; padding: 8px; text-align: left;">${col}</th>`).join('')}
        //             </tr>
        //         </thead>
        //         <tbody>
        //             ${rows
        //                 .map(
        //                     (row) => `
        //                 <tr>
        //                     ${row.map((col) => `<td style="border: 1px solid #ddd; padding: 8px;">${col}</td>`).join('')}
        //                 </tr>
        //             `
        //                 )
        //                 .join('')}
        //         </tbody>
        //     </table>
        // `;

        //         return tableHTML;
        //     }
        // );

        // Change special symbols
        text = symbolReplacements(text);
        return text;
    };

    const processedContent = preprocessContent(content);

    useEffect(() => {
        hljs.highlightAll.called = false;
        hljs?.highlightAll();

        const tables = document.querySelectorAll('table');
        tables.forEach((table) => {
            const rows = table.querySelectorAll('tbody tr');
            rows.forEach((row, index) => {
                if ((index + 1) % 2 === 0) {
                    row.style.backgroundColor = `${primaryBlue.c100}80`;
                }
            });
        });
    }, [processedContent]);

    // GovTool markdown values have no trailing gap under the last block.
    return (
        <Box
            data-testid={testId}
            className='markdown'
            sx={{ '& > :last-child': { marginBottom: '0 !important' } }}
        >
            <ReactMarkdown
                components={typographyComponents}
                key={testId}
                rehypePlugins={[rehypeRaw, rehypeKatex]}
                remarkPlugins={[remarkBreaks, remarkMath, remarkGfm]}
            >
                {processedContent}
            </ReactMarkdown>
        </Box>
    );
};

export default MarkdownTypography;
