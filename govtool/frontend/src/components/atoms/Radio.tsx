/* eslint-disable @typescript-eslint/no-explicit-any */
import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { UseFormRegister, UseFormSetValue } from "react-hook-form";
import { theme } from "@/theme";

type RadioProps = {
  isChecked: boolean;
  name: string;
  title: string;
  value: string;
  setValue: UseFormSetValue<any>;
  register: UseFormRegister<any>;
  dataTestId?: string;
  disabled?: boolean;
};

export const Radio = ({ ...props }: RadioProps) => {
  const {
    isChecked,
    name,
    setValue,
    title,
    value,
    dataTestId,
    register,
    disabled,
  } = props;

  const handleClick = () => {
    setValue(name, value);
  };

  return (
    <Box
      data-testid={dataTestId}
      onClick={() => {
        if (!disabled) handleClick();
      }}
      borderRadius={isChecked ? "15px" : "12px"}
      p={isChecked ? "2px" : 0}
      border={isChecked ? 2 : 0}
      borderColor={isChecked ? "specialCyanBorder" : undefined}
      sx={[
        {
          boxShadow: theme.shadows[1],

          "&:hover": {
            color: "blue",
            cursor: disabled ? "default" : "pointer",
          },
        },
      ]}
    >
      <input
        type="radio"
        value={value}
        {...register(name)}
        style={{ display: "none" }}
        checked={isChecked}
      />
      <Box
        borderRadius="12px"
        bgcolor={isChecked ? "specialCyan" : "white"}
        py={1.5}
      >
        <Typography
          variant="body1"
          sx={{
            textAlign: "center",
            color: isChecked ? "white" : "textBlack",
          }}
        >
          {title}
        </Typography>
      </Box>
    </Box>
  );
};
