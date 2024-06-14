import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";
import Markdown from "react-markdown";

import { Typography, Tooltip, CopyButton, TooltipProps } from "@atoms";

type BaseProps = {
  label: string;
  text?: string | number;
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

type GovernanceActionCardElementProps = (
  | PillVariantProps
  | OtherVariantsProps
) & { isMarkdown?: boolean };

export const GovernanceActionCardElement = ({
  label,
  text,
  dataTestId,
  isSliderCard,
  textVariant = "oneLine",
  isCopyButton,
  tooltipProps,
  marginBottom,
  isMarkdown = false,
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
            {isMarkdown ? (
              <Markdown
                components={{
                  // eslint-disable-next-line
                  p(props) {
                    const { children } = props;
                    return (
                      <Typography
                        sx={{
                          fontSize: 16,
                          fontWeight: 400,
                          lineHeight: "24px",
                          maxWidth: "auto",
                        }}
                      >
                        {children}
                      </Typography>
                    );
                  },
                }}
              >
                {text.toString()}
              </Markdown>
            ) : (
              <Typography
                sx={{
                  fontSize: isSliderCard ? 14 : 16,
                  fontWeight: 400,
                  maxWidth: textVariant === "oneLine" ? "283px" : "auto",
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
            )}
            {isCopyButton && (
              <Box ml={1}>
                <CopyButton text={text.toString()} variant="blueThin" />
              </Box>
            )}
          </Box>
        )}
      </Box>
    </Box>
  );
};
