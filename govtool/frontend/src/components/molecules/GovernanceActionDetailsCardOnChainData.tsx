import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";

type GovernanceActionDetailsCardOnChainDataProps = {
  data: {
    label: string;
    content: string;
  }[];
};

export const GovernanceActionDetailsCardOnChainData = ({
  data,
}: GovernanceActionDetailsCardOnChainDataProps) => {
  const { t } = useTranslation();

  return (
    <Box mb="32px" data-testid="governance-action-on-chain-data">
      <Box
        sx={{
          display: "flex",
          alignItems: "center",
          mb: "4px",
        }}
      >
        <Typography
          sx={{
            fontSize: 14,
            fontWeight: 600,
            lineHeight: "20px",
            color: "neutralGray",
            overflow: "hidden",
            textOverflow: "ellipsis",
            whiteSpace: "nowrap",
          }}
        >
          {t("govActions.onChainTransactionDetails")}
        </Typography>
      </Box>
      {data.map(({ label, content }) => (
        <Box
          key={label}
          sx={{
            display: "flex",
            alignItems: "center",
            overflow: "hidden",
          }}
        >
          <Typography
            sx={{
              fontSize: 16,
              fontWeight: 600,
              lineHeight: "24px",
              whiteSpace: "nowrap",
              overflow: "hidden",
              textOverflow: "ellipsis",
            }}
          >
            {label}:
          </Typography>
          <Typography
            sx={{
              fontSize: 16,
              fontWeight: 400,
              lineHeight: "24px",
              whiteSpace: "nowrap",
              overflow: "hidden",
              textOverflow: "ellipsis",
              ml: 0.5,
            }}
          >
            {content}
          </Typography>
        </Box>
      ))}
    </Box>
  );
};
