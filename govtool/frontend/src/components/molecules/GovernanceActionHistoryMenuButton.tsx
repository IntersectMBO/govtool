import { MouseEvent, useState } from "react";
import { Badge, Box, Button } from "@mui/material";
import {
  IconCheveronDown,
  IconCheveronUp,
} from "@intersect.mbo/intersectmbo.org-icons-set";

import { Typography } from "@atoms";

type GovernanceActionHistoryMenuButtonProps = {
  id: string;
  menuId: string;
  label: string;
  isOpen: boolean;
  badgeCount?: number;
  onClick: (event: MouseEvent<HTMLElement>) => void;
};

/** The white, bordered trigger of the governanceActionHistory filter and sort menus. */
export const GovernanceActionHistoryMenuButton = ({
  id,
  menuId,
  label,
  isOpen,
  badgeCount,
  onClick,
}: GovernanceActionHistoryMenuButtonProps) => {
  const [isHovered, setIsHovered] = useState(false);

  return (
    <Button
      id={id}
      data-testid={id}
      aria-controls={isOpen ? menuId : undefined}
      aria-haspopup="true"
      aria-expanded={isOpen ? "true" : undefined}
      sx={{
        position: "relative",
        backgroundColor: "white",
        border: 1,
        borderColor: "#BFC8D961",
        borderRadius: 1,
        height: 48,
        paddingRight: "4px",
        paddingLeft: "12px",
      }}
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
      onClick={onClick}
    >
      {!isOpen && !!badgeCount && (
        <Badge
          badgeContent={badgeCount}
          color="secondary"
          sx={{
            position: "absolute",
            top: 2,
            right: 0,
            "& .MuiBadge-badge": { color: "white" },
          }}
        />
      )}
      <Box sx={{ display: "flex", alignItems: "center", gap: 1.25 }}>
        <Typography
          sx={{
            color: isHovered || isOpen ? "textBlack" : "#506288",
            fontSize: 14,
            fontWeight: 400,
            paddingX: 0.5,
            whiteSpace: "nowrap",
          }}
        >
          {label}
        </Typography>
        <Box
          sx={{
            alignItems: "center",
            borderRadius: "50%",
            display: "flex",
            height: 28,
            justifyContent: "center",
            width: 28,
            "&:hover": { backgroundColor: "action.hover" },
          }}
        >
          {isOpen ? (
            <IconCheveronUp width={18} height={18} />
          ) : (
            <IconCheveronDown width={18} height={18} />
          )}
        </Box>
      </Box>
    </Button>
  );
};
