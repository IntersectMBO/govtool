import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";

import { Typography, CopyButton } from "@atoms";
import { correctVoteAdaFormat } from "@utils";

import { useScreenDimension } from "@/hooks";

type Props = {
  receivingAddress: string;
  amount: number;
};

export const GovernanceActionCardTreasuryWithdrawalElement = ({
  receivingAddress,
  amount,
}: Props) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  return (
    <Box
      sx={{
        display: "flex",
        mb: "4px",
        flexDirection: "column",
      }}
    >
      <Box sx={{ display: "flex", flexDirection: isMobile ? "column" : "row" }}>
        <Typography
          data-testid="receiving-address-label"
          sx={{
            width: "160px",
            fontSize: 14,
            fontWeight: 600,
            lineHeight: "20px",
            color: "neutralGray",
          }}
        >
          {t("govActions.receivingAddress")}
        </Typography>
        <Box
          sx={{
            display: "flex",
            alignItems: "center",
            overflow: "hidden",
            flexDirection: "row",
          }}
        >
          <Typography
            data-testid="receiving-address"
            sx={{
              ml: isMobile ? 0 : 8,
              color: "primaryBlue",
              fontSize: 16,
              fontWeight: 400,
              lineHeight: "20px",
              whiteSpace: "nowrap",
              overflow: "hidden",
              textOverflow: "ellipsis",
            }}
          >
            {receivingAddress}
          </Typography>
          <Box ml={1}>
            <CopyButton text={receivingAddress} variant="blueThin" />
          </Box>
        </Box>
      </Box>
      <Box
        sx={{
          display: "flex",
          flexDirection: isMobile ? "column" : "row",
          mt: "6px",
        }}
      >
        <Typography
          sx={{
            width: "160px",
            fontSize: 14,
            fontWeight: 600,
            lineHeight: "20px",
            color: "neutralGray",
          }}
          data-testid="amount-label"
        >
          {t("govActions.amount")}
        </Typography>
        <Typography
          data-testid="amount"
          sx={{
            ml: isMobile ? 0 : 8,
            fontSize: 16,
            fontWeight: 400,
            lineHeight: "20px",
          }}
        >
          ₳ {correctVoteAdaFormat(amount) ?? 0}
        </Typography>
      </Box>
    </Box>
  );
};
