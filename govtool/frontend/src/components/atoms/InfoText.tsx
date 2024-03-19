import { InfoTextProps, Typography } from ".";

export function InfoText({ label, sx }: InfoTextProps) {
  return (
    <Typography color="#FF833B" sx={sx} variant="body1">
      {label.toLocaleUpperCase()}
    </Typography>
  );
}
