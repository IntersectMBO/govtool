import { Typography } from ".";

export type InfoTextProps = {
  children: string;
};

export const InfoText = ({ children }: InfoTextProps) => {
  return (
    <Typography color="#FF833B" variant="body1">
      {children.toLocaleUpperCase()}
    </Typography>
  );
};
