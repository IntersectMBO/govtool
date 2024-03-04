import { Box } from "@mui/material";
import ChevronRightIcon from "@mui/icons-material/ChevronRight";

import { theme } from "@/theme";

interface Props {
  disabled: boolean;
  onClick: (e: any) => void;
  left?: boolean;
}

export const SliderArrow = ({ disabled, onClick, left }: Props) => {
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
