import * as React from "react";
import { Chip, ChipProps, IconButton } from "@mui/material";
import { IconX } from "@intersect.mbo/intersectmbo.org-icons-set";

interface ChipButtonProps
  extends Omit<ChipProps, "color" | "variant" | "deleteIcon"> {
  label: React.ReactNode;
  onDelete: () => void;
  bgColor?: string;
  deleteIconPosition?: "left" | "right";
  iconSize?: number;
  testId?: string;
}

const ChipButton: React.FC<ChipButtonProps> = ({
  label,
  onDelete,
  bgColor = "#B9CCF5",
  deleteIconPosition = "left",
  iconSize = 14,
  testId,
  sx,
  ...rest
}) => (
  <Chip
    {...rest}
    label={label}
    onDelete={onDelete}
    data-testid={testId}
    deleteIcon={
      <IconButton
        disableRipple
        sx={{ p: "4px", "&:hover": { backgroundColor: "transparent" } }}
      >
        <IconX style={{ width: iconSize, height: iconSize, color: "#222" }} />
      </IconButton>
    }
    sx={{
      backgroundColor: bgColor,
      borderRadius: 999,
      height: "auto",
      py: 0.75,
      pl: 1.75,
      pr: 2.25,
      display: "flex",
      flexDirection: deleteIconPosition === "right" ? "row" : "row-reverse",
      gap: 0.5,
      "& .MuiChip-label": {
        fontSize: 12,
        fontWeight: 400,
        color: "#000",
        whiteSpace: "nowrap",
        overflow: "hidden",
        textOverflow: "ellipsis",
        px: 0,
        py: 0,
      },
      "& .MuiChip-deleteIcon": { m: 0 },
      ...(sx as object),
    }}
  />
);

export default ChipButton;
