import { Box } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";
import Markdown from "react-markdown";

import { Typography, Tooltip, CopyButton, TooltipProps } from "@atoms";
import { removeMarkdown } from "@/utils";
import { ICONS } from "@/consts";
import { useModal } from "@/context";

type BaseProps = {
  label: string;
  text?: string | number;
  dataTestId?: string;
  isSliderCard?: boolean;
  tooltipProps?: Omit<TooltipProps, "children">;
  marginBottom?: number;
  isSemiTransparent?: boolean;
};

type PillVariantProps = BaseProps & {
  textVariant: "pill";
  isCopyButton?: false;
  isLinkButton?: false;
};

type OtherVariantsProps = BaseProps & {
  textVariant?: "oneLine" | "twoLines" | "longText";
  isCopyButton?: boolean;
  isLinkButton?: boolean;
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
  isLinkButton,
  tooltipProps,
  marginBottom,
  isMarkdown = false,
  isSemiTransparent = false,
}: GovernanceActionCardElementProps) => {
  const { openModal } = useModal();

  if (!text) {
    return null;
  }
  return (
    <Box
      data-testid={dataTestId}
      mb={marginBottom ?? isSliderCard ? "20px" : "32px"}
      maxHeight={isSliderCard ? "72px" : "none"}
      overflow={isSliderCard ? "hidden" : "visible"}
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
              alignItems: isMarkdown ? "unset" : "center",
              overflow: "hidden",
              flexDirection: isMarkdown ? "column" : "row",
              // Workaround for not setting the font in markdown
              fontFamily: "Poppins, Arial",
            }}
          >
            {!isSliderCard && isMarkdown ? (
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
                  ...((isCopyButton || isLinkButton) && {
                    color: "primaryBlue",
                  }),
                  ...(isSemiTransparent && {
                    opacity: 0.75,
                  }),
                }}
              >
                {isMarkdown ? removeMarkdown(text) : text}
              </Typography>
            )}
            {isCopyButton && (
              <Box ml={1}>
                <CopyButton text={text.toString()} variant="blueThin" />
              </Box>
            )}
            {isLinkButton && (
              <Box ml={1}>
                <img
                  data-testid="link-button"
                  alt="link"
                  src={ICONS.externalLinkIcon}
                  style={{ cursor: "pointer" }}
                  onClick={() => {
                    openModal({
                      type: "externalLink",
                      state: {
                        externalLink: text.toString(),
                      },
                    });
                  }}
                />
              </Box>
            )}
          </Box>
        )}
      </Box>
    </Box>
  );
};
