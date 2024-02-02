import { Box, Typography } from "@mui/material";
import { UseFormRegister, UseFormSetValue } from "react-hook-form";

type RadioProps = {
  isChecked: boolean;
  name: string;
  title: string;
  value: string;
  setValue: UseFormSetValue<any>;
  register: UseFormRegister<any>;
  dataTestId?: string;
};

export const Radio = ({ ...props }: RadioProps) => {
  const { isChecked, name, setValue, title, value, dataTestId, register } =
    props;

  const handleClick = () => {
    setValue(name, value);
  };

  return (
    <Box
      data-testid={dataTestId}
      onClick={handleClick}
      borderRadius={2}
      p={0.2}
      border={isChecked ? 1 : 0}
      borderColor={isChecked ? "specialCyanBorder" : "white"}
      sx={[{ "&:hover": { color: "blue", cursor: "pointer" } }]}
      flex={1}
    >
      <input
        type="radio"
        value={value}
        {...register(name)}
        style={{ display: "none" }}
        checked={isChecked}
      />
      <Box
        borderRadius={1.5}
        bgcolor={isChecked ? "specialCyan" : "white"}
        py={1.5}
        border={isChecked ? 0 : 1}
        borderColor={"lightBlue"}
      >
        <Typography
          textAlign="center"
          color={isChecked ? "white" : "textBlack"}
        >
          {title}
        </Typography>
      </Box>
    </Box>
  );
};
