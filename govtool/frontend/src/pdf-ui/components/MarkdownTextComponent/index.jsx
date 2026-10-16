import { Typography } from '@atoms';

const removeMarkdown = (markdown) => {
    if (!markdown) return '';

    return markdown
        .replace(/(\*\*|__)(.*?)\1/g, '$2')
        .replace(/(\*|_)(.*?)\1/g, '$2')
        .replace(/\~\~(.*?)\~\~/g, '$1')
        .replace(/\!\[.*?\]\(.*?\)/g, '')
        .replace(/\[(.*?)\]\(.*?\)/g, '$1')
        .replace(/`{1,2}([^`]+)`{1,2}/g, '$1')
        .replace(/^\s{0,3}>\s?/g, '')
        .replace(/^\s{1,3}([-*+]|\d+\.)\s+/g, '')
        .replace(/^(\n)?\s{0,}#{1,6}\s*( (.+))? +#+$|^(\n)?\s{0,}#{1,6}\s*( (.+))?$/gm, '$1$3$4$6')
        .replace(/\n{2,}/g, '\n')
        .replace(/\\([\\`*{}\[\]()#+\-.!_>])/g, '$1');
};

const MarkdownTextComponent = ({ markdownText }) => {
    const plainText = removeMarkdown(markdownText);

    return (
        <Typography
            variant='body2'
            component='p'
            fontWeight={400}
            color='textBlack'
            sx={{
                display: '-webkit-box',
                WebkitBoxOrient: 'vertical',
                WebkitLineClamp: 3,
                overflow: 'hidden',
                textOverflow: 'ellipsis',
                whiteSpace: 'normal',
                // GovTool's slider-card text: 14px on a 20px line.
                lineHeight: '20px',
                maxHeight: '60px',
            }}
        >
            {plainText}
        </Typography>
    );
};

export default MarkdownTextComponent;
