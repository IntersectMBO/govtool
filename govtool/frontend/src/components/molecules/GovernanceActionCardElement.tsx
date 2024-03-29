import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Typography, Tooltip, CopyButton, TooltipProps } from "@atoms";

type BaseProps = {
  label: string;
  text?: string;
  dataTestId?: string;
  isSliderCard?: boolean;
  tooltipProps?: Omit<TooltipProps, "children">;
  marginBottom?: number;
};

type PillVariantProps = BaseProps & {
  textVariant: "pill";
  isCopyButton?: false;
};

type OtherVariantsProps = BaseProps & {
  textVariant?: "oneLine" | "twoLines" | "longText";
  isCopyButton?: boolean;
};

type GovernanceActionCardElementProps = PillVariantProps | OtherVariantsProps;

export const GovernanceActionCardElement = ({
  label,
  text,
  dataTestId,
  isSliderCard,
  textVariant = "oneLine",
  isCopyButton,
  tooltipProps,
  marginBottom,
}: GovernanceActionCardElementProps) => {
  if (!text) {
    return null;
  }
  return (
    <Box
      data-testid={dataTestId}
      mb={marginBottom ?? isSliderCard ? "20px" : "32px"}
    >
      <Box
        sx={{
          display: "flex",
          alignItems: "center",
          mb: "4px",
        }}
      >
        <Typography
          sx={{
            fontSize: isSliderCard ? 12 : 14,
            fontWeight: isSliderCard ? 500 : 600,
            lineHeight: isSliderCard ? "16px" : "20px",
            color: "neutralGray",
            overflow: "hidden",
            textOverflow: "ellipsis",
            whiteSpace: "nowrap",
          }}
        >
          {label}
        </Typography>
        {tooltipProps && (
          <Tooltip
            heading={tooltipProps?.heading}
            paragraphOne={tooltipProps?.paragraphOne}
            placement="bottom-end"
            arrow
            {...tooltipProps}
          >
            <InfoOutlinedIcon
              sx={{ ml: 0.7, mb: 0.1, color: "#ADAEAD", fontSize: "small" }}
            />
          </Tooltip>
        )}
      </Box>
      <Box display="flex">
        {textVariant === "pill" ? (
          <Box
            sx={{
              padding: "6px 18px",
              overflow: "hidden",
              bgcolor: "lightBlue",
              borderRadius: 100,
            }}
          >
            <Typography
              variant="caption"
              sx={{
                overflow: "hidden",
                textOverflow: "ellipsis",
                whiteSpace: "nowrap",
              }}
            >
              {text}
            </Typography>
          </Box>
        ) : (
          <Box
            sx={{
              display: "flex",
              alignItems: "center",
              overflow: "hidden",
            }}
          >
            <Typography
              sx={{
                fontSize: isSliderCard ? 14 : 16,
                fontWeight: 400,
                lineHeight: isSliderCard ? "20px" : "24px",
                ...(textVariant === "oneLine" && { whiteSpace: "nowrap" }),
                ...((textVariant === "oneLine" ||
                  textVariant === "twoLines") && {
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                }),
                ...(textVariant === "twoLines" && {
                  display: "-webkit-box",
                  WebkitBoxOrient: "vertical",
                  WebkitLineClamp: 2,
                  whiteSpace: "normal",
                }),
                ...(isCopyButton && {
                  color: "primaryBlue",
                }),
              }}
            >
              {text}
            </Typography>
            {isCopyButton && (
              <Box ml={1}>
                <CopyButton text={text} variant="blueThin" />
              </Box>
            )}
          </Box>
        )}
      </Box>
    </Box>
  );
};
