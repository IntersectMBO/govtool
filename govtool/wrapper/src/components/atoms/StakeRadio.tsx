import { Dispatch, FC, SetStateAction } from "react";
import { Box, IconButton, Typography } from "@mui/material";

import { ICONS } from "@consts";
import {
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { theme } from "@/theme";
import { correctAdaFormat } from "@/utils/adaFormat";

type StakeRadioProps = {
  isChecked?: boolean;
  stakeKey: string;
  onChange: Dispatch<SetStateAction<string>>;
  dataTestId?: string;
};

export const StakeRadio: FC<StakeRadioProps> = ({ ...props }) => {
  const { dataTestId, isChecked = false, stakeKey, onChange } = props;
  const {
    palette: { boxShadow1 },
  } = theme;
  const { isMobile } = useScreenDimension();
  const { powerIsLoading, votingPower } =
    useGetAdaHolderVotingPowerQuery(stakeKey);
  const { t } = useTranslation();

  return (
    <Box
      data-testid={dataTestId}
      border={isChecked ? 2 : 1}
      p="2px"
      bgcolor="white"
      borderColor={isChecked ? "specialCyanBorder" : "#D6E2FF"}
      borderRadius="15px"
      boxShadow={`1px 2px 11px 0px ${boxShadow1}`}
      onClick={() => onChange(stakeKey)}
      sx={[{ "&:hover": { cursor: "pointer" } }]}
    >
      <Box
        px="18px"
        py="14px"
        bgcolor={isChecked ? "specialCyan" : "white"}
        borderRadius="12px"
      >
        <Box alignItems="center" display="flex" justifyContent="space-between">
          <Typography
            color={isChecked ? "white" : "textBlack"}
            fontSize={14}
            fontWeight={isChecked ? 600 : 400}
            overflow="hidden"
            textOverflow="ellipsis"
            width={isMobile ? "55vw" : "20vw"}
            mr={isMobile ? 2 : 8}
          >
            {stakeKey}
          </Typography>
          <IconButton color="primary">
            <img
              alt="copy"
              onClick={(e) => {
                navigator.clipboard.writeText(stakeKey);
                e.stopPropagation();
              }}
              src={isChecked ? ICONS.copyWhiteIcon : ICONS.copyIcon}
            />
          </IconButton>
        </Box>
        <Box alignItems="center" display="flex">
          <Typography color={isChecked ? "white" : "#8E908E"} variant="body2">
            {t("votingPower")}
            :
          </Typography>
          {powerIsLoading ? (
            <Typography color={isChecked ? "white" : "#8E908E"} variant="body2">
              {t("loading")}
            </Typography>
          ) : (
            <Typography
              color={isChecked ? "white" : "#001A57"}
              fontWeight={600}
              marginLeft="4px"
            >
              ₳ {correctAdaFormat(votingPower) ?? 0}
            </Typography>
          )}
        </Box>
      </Box>
    </Box>
  );
};
