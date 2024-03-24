import { Box } from "@mui/material";
import ChevronRightIcon from "@mui/icons-material/ChevronRight";

import { theme } from "@/theme";

interface SliderArrowProps {
  disabled: boolean;
  onClick: (e: React.MouseEvent<HTMLDivElement, MouseEvent>) => void;
  left?: boolean;
}

export const SliderArrow = ({ disabled, onClick, left }: SliderArrowProps) => {
  const {
    palette: { primaryBlue, arcticWhite, lightBlue },
  } = theme;

  return (
    <Box
      onClick={onClick}
      sx={{
        width: "44px",
        height: "44px",
        borderRadius: "50%",
        border: `1px solid ${lightBlue}`,
        backgroundColor: arcticWhite,
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        cursor: "pointer",
        transition: "0.3s",

        "&:hover": {
          boxShadow: disabled ? 0 : 2,
        },
      }}
    >
      <ChevronRightIcon
        sx={{
          transform: `rotate(${left ? 180 : 0}deg)`,
          color: disabled ? "#C1BED3" : primaryBlue,
        }}
      />
    </Box>
  );
};
