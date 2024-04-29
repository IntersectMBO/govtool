import { InfoTextProps, Typography } from ".";

export const InfoText = ({ label, sx }: InfoTextProps) => (
  <Typography color="#FF833B" sx={sx} variant="body1">
    {label.toLocaleUpperCase()}
  </Typography>
);
