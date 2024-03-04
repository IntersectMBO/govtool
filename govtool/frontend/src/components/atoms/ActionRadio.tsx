import { FC } from "react";
import { Box, Typography } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Tooltip } from ".";
import { useScreenDimension } from "@hooks";
import { theme } from "@/theme";

type ActionRadioProps = {
  dataTestId?: string;
  isChecked?: boolean;
  onChange: (newValue: string) => void;
  subtitle?: string;
  title: string;
  tooltipText?: string;
  tooltipTitle?: string;
  value: string;
};

export const ActionRadio: FC<ActionRadioProps> = ({ ...props }) => {
  const {
    dataTestId,
    isChecked = false,
    onChange,
    subtitle,
    title,
    tooltipText,
    tooltipTitle,
    value,
  } = props;
  const {
    palette: { boxShadow1 },
  } = theme;
  const { isMobile } = useScreenDimension();

  return (
    <Box
      data-testid={dataTestId}
      aria-checked={isChecked}
      p="2px"
      maxWidth={"623px"}
      border="2px solid"
      borderColor={isChecked ? "specialCyanBorder" : "white"}
      bgcolor={"white"}
      borderRadius="15px"
      boxShadow={`1px 2px 11px 0px ${boxShadow1}`}
      onClick={() => onChange(value)}
      sx={[{ "&:hover": { cursor: "pointer" } }]}
    >
      <Box
        p={2}
        bgcolor={isChecked ? "specialCyan" : "white"}
        borderRadius={"12px"}
      >
        <Box display="flex" flexDirection="row" alignItems="center">
          <Typography
            color={isChecked ? "white" : "textBlack"}
            fontWeight={600}
          >
            {title}
          </Typography>
          {tooltipText && (
            <Tooltip
              heading={tooltipTitle}
              paragraphOne={tooltipText}
              placement={isMobile ? "top" : "right"}
              arrow
            >
              <InfoOutlinedIcon
                style={{
                  color: isChecked ? "white" : "#ADAEAD",
                }}
                sx={{ ml: 1 }}
                fontSize="small"
              />
            </Tooltip>
          )}
        </Box>
        {subtitle ? (
          <Typography
            variant="body2"
            mt={1}
            color={isChecked ? "white" : "textBlack"}
          >
            {subtitle}
          </Typography>
        ) : null}
      </Box>
    </Box>
  );
};
