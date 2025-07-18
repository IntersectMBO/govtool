import { PropsWithChildren } from "react";
import { Box, Skeleton } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";
import Markdown from "react-markdown";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import "katex/dist/katex.min.css";
import "./tableMarkdown.css";

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
  isValidating?: boolean;
  children?: React.ReactNode;
};

type VariantProps = BaseProps & {
  textVariant?: "pill" | "oneLine" | "twoLines" | "longText";
  isCopyButton?: boolean;
  isLinkButton?: boolean;
  isMarkdown?: boolean;
};

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
  isValidating = false,
  children,
}: VariantProps) => {
  const { openModal } = useModal();

  if (text == null && children == null) return null;

  const renderTooltip = () =>
    tooltipProps && (
      <Tooltip
        heading={tooltipProps.heading}
        paragraphOne={tooltipProps.paragraphOne}
        placement="bottom-end"
        arrow
        {...tooltipProps}
      >
        <InfoOutlinedIcon
          sx={{ ml: 0.7, mb: 0.1, color: "#ADAEAD", fontSize: "small" }}
        />
      </Tooltip>
    );

  const renderPillText = () => (
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
  );

  const renderStandardText = () => (
    <Typography
      sx={{
        fontSize: isSliderCard ? 14 : 16,
        fontWeight: 400,
        maxWidth: textVariant === "oneLine" ? "283px" : "auto",
        lineHeight: isSliderCard ? "20px" : "24px",
        ...(textVariant === "oneLine" && { whiteSpace: "nowrap" }),
        ...((textVariant === "oneLine" || textVariant === "twoLines") && {
          overflow: "hidden",
          textOverflow: "ellipsis",
        }),
        ...(textVariant === "twoLines" && {
          display: "-webkit-box",
          WebkitBoxOrient: "vertical",
          WebkitLineClamp: 2,
          whiteSpace: "normal",
        }),
        ...((isCopyButton || isLinkButton) && { color: "primaryBlue" }),
        ...(isSemiTransparent && { opacity: 0.75 }),
      }}
    >
      {isMarkdown ? removeMarkdown(text) : text}
    </Typography>
  );

  const renderMarkdownText = ({
    children: markdownChildren,
  }: PropsWithChildren) => (
    <Typography
      sx={{
        fontSize: 16,
        fontWeight: 400,
        lineHeight: "24px",
        maxWidth: "auto",
      }}
    >
      {markdownChildren}
    </Typography>
  );

  const markdownComponents = {
    p: ({ children: markdownChildren }: PropsWithChildren) =>
      renderMarkdownText({ children: markdownChildren }),
    br: () => <br />,
    img: ({
      src,
      alt,
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      node, // node is passed by react-markdown but we should not use it
      ...rest
    }: React.ImgHTMLAttributes<HTMLImageElement> & { node?: unknown }) => (
      <img
        {...rest}
        src={src ?? ""}
        alt={alt ?? ""}
        style={{ maxWidth: "100%", height: "auto", display: "block" }}
        loading="lazy"
      />
      ),
  };

  const renderMarkdown = (markdownText: string | number) => {
    const formattedText = markdownText
      .toString()
      .replace(/\r\n|\r/g, "\n")
      .replace(
        /\n\n+/g,
        (match) =>
          `\n\n${Array(match.length - 1)
            .fill("&nbsp;  \n")
            .join("")}\n`,
      )
      .split("\n")
      .map((line) => `${line}  `)
      .join("\n");

    return (
      <Markdown
        className="markdown"
        components={markdownComponents}
        remarkPlugins={[remarkMath, remarkGfm]}
        rehypePlugins={[rehypeKatex]}
      >
        {formattedText}
      </Markdown>
    );
  };

  const renderCopyButton = (copyText: string | number) =>
    isCopyButton && (
      <Box ml={1}>
        <CopyButton text={copyText.toString()} variant="blueThin" />
      </Box>
    );

  const renderLinkButton = (linkText: string | number) =>
    isLinkButton && (
      <Box ml={1}>
        <img
          data-testid="link-button"
          alt="link"
          src={ICONS.externalLinkIcon}
          style={{ cursor: "pointer" }}
          onClick={() =>
            openModal({
              type: "externalLink",
              state: { externalLink: linkText.toString() },
            })
          }
        />
      </Box>
    );

  const renderTextOrChildren = () => {
    if (text != null) {
      return (
        <>
          {textVariant === "pill"
            ? renderPillText()
            : isMarkdown && !isSliderCard
            ? renderMarkdown(text)
            : renderStandardText()}
          {renderCopyButton(text)}
          {renderLinkButton(text)}
        </>
      );
    }
    if (children != null) {
      return children;
    }
  };

  return (
    <Box
      data-testid={dataTestId}
      mb={marginBottom ?? (isSliderCard ? "20px" : "32px")}
      maxHeight={isSliderCard ? "72px" : "none"}
      overflow={isSliderCard ? "hidden" : "visible"}
    >
      <Box sx={{ display: "flex", alignItems: "center", mb: "4px" }}>
        {isValidating ? (
          <Skeleton height="16px" width="64px" variant="text" />
        ) : (
          <Typography
            component="h2"
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
        )}
        {renderTooltip()}
      </Box>
      {isValidating ? (
        <Skeleton height="32px" width="100%" variant="text" />
      ) : (
        <Box
          display="flex"
          alignItems={isMarkdown ? "unset" : "center"}
          overflow="hidden"
          flexDirection={isMarkdown ? "column" : "row"}
          fontFamily="Poppins, Arial"
        >
          {renderTextOrChildren()}
        </Box>
      )}
    </Box>
  );
};
