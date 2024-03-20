import { Typography } from '@mui/material';

interface Props {
  text: string;
  searchPhrase?: string;
  fontSize?: number;
  fontWeight?: number | string;
  color?: string;
}

export const HighlightedText = ({
  text,
  searchPhrase = '',
  fontSize = 12,
  fontWeight = 400,
  color = 'inherit',
}: Props) => {
  const regex = new RegExp(`(${searchPhrase})`, 'gi');
  const parts = text.split(regex);

  return (
    <>
      {parts.map((part, index) => (
        <Typography
          key={index}
          component="span"
          fontSize={fontSize}
          fontWeight={fontWeight}
          lineHeight="16px"
          style={{
            backgroundColor:
              part.toLowerCase() === searchPhrase.toLowerCase()
                ? '#FF640A'
                : 'transparent',
            color:
              part.toLowerCase() === searchPhrase.toLowerCase()
                ? 'white'
                : `${color}`,
          }}
        >
          {part}
        </Typography>
      ))}
    </>
  );
};
