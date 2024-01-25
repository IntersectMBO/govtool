import { useMemo, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Button, Grid, Typography } from "@mui/material";

import { StakeRadio } from "@atoms";
import { useCardano, useSnackbar } from "@context";
import { PATHS } from "@consts";
import { setItemToLocalStorage, WALLET_LS_KEY } from "@utils";
import { theme } from "@/theme";
import { useScreenDimension } from "@hooks";
import { usei18n } from "@translations";

export const ChooseStakeKeyPanel = () => {
  const { disconnectWallet, stakeKeys, setStakeKey } = useCardano();
  const navigate = useNavigate();
  const { addSuccessAlert } = useSnackbar();
  const [chosenKey, setChosenKey] = useState<string>("");
  const { isMobile } = useScreenDimension();
  const { t } = usei18n();
  const {
    palette: { boxShadow2 },
  } = theme;

  const renderCancelButton = useMemo(() => {
    return (
      <Button
        data-testid={"cancel-button"}
        variant="outlined"
        onClick={() => {
          disconnectWallet();
          navigate(PATHS.home);
        }}
        sx={{
          borderRadius: 50,
          textTransform: "none",
          width: isMobile ? "100%" : "auto",
          height: 48,
          px: isMobile ? 0 : 6,
        }}
      >
        {t("cancel")}
      </Button>
    );
  }, [isMobile]);

  const renderSelectButton = useMemo(() => {
    return (
      <Button
        data-testid={"select-button"}
        disabled={!chosenKey}
        onClick={() => {
          setStakeKey(chosenKey);
          setItemToLocalStorage(`${WALLET_LS_KEY}_stake_key`, chosenKey);
          addSuccessAlert(`Wallet connected`, 3000);
          navigate(PATHS.dashboard);
        }}
        sx={{
          borderRadius: 50,
          textTransform: "none",
          width: isMobile ? "100%" : "auto",
          px: isMobile ? 0 : 6,
          height: 48,
        }}
        variant="contained"
      >
        {t("select")}
      </Button>
    );
  }, [isMobile, chosenKey, setStakeKey]);

  return (
    <Box
      display="flex"
      justifyContent="center"
      flexDirection="column"
      width={isMobile ? "100%" : "auto"}
    >
      <Box
        borderRadius={"20px"}
        boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
        mt={isMobile ? 2 : 7}
        mb={isMobile ? 0 : 6}
        px={isMobile ? 2 : 7.3}
        py={isMobile ? 4 : 6}
      >
        <Box>
          <Typography fontSize={32} fontWeight={600} textAlign={"center"}>
            {t("wallet.pickStakeKey")}
          </Typography>
          <Typography fontSize={14} textAlign={"center"} mt={1} mb={4}>
            {t("wallet.pickStakeKey")}
          </Typography>
          <Grid
            container
            columns={1}
            display={"flex"}
            flexDirection={"column"}
            rowGap={3}
          >
            {stakeKeys.map((k) => {
              return (
                <Grid key={k} item flex={1}>
                  <StakeRadio
                    dataTestId={k + "-radio"}
                    key={k}
                    onChange={setChosenKey}
                    isChecked={chosenKey === k}
                    stakeKey={k}
                  />
                </Grid>
              );
            })}
          </Grid>
        </Box>
        <Box
          display={"flex"}
          flexDirection={isMobile ? "column" : "row"}
          justifyContent={"space-between"}
          mt={6}
        >
          {isMobile ? renderSelectButton : renderCancelButton}
          <Box px={2} py={isMobile ? 1.5 : 0} />
          {isMobile ? renderCancelButton : renderSelectButton}
        </Box>
      </Box>
    </Box>
  );
};
