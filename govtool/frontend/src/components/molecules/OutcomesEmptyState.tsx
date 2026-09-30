import { Card } from "@mui/material";

import { Typography } from "@atoms";

type OutcomesEmptyStateProps = {
  title: string;
  description: string;
};

export const OutcomesEmptyState = ({
  title,
  description,
}: OutcomesEmptyStateProps) => (
  <Card
    variant="outlined"
    elevation={0}
    sx={{
      alignItems: "center",
      display: "flex",
      flexDirection: "column",
      gap: 1,
      py: 5,
      px: 1,
      textAlign: "center",
      width: "-webkit-fill-available",
    }}
  >
    <Typography fontSize={22} fontWeight={500}>
      {title}
    </Typography>
    <Typography fontWeight={400}>{description}</Typography>
  </Card>
);
