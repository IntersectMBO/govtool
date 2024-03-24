/* eslint-disable @typescript-eslint/no-explicit-any */
import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { UseFormRegister, UseFormSetValue } from "react-hook-form";

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
          boxShadow:
            "0px 1px 2px 0px rgba(0, 51, 173, 0.08), 0px 1px 6px 1px rgba(0, 51, 173, 0.15)",

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
