import { Box, Link, Typography } from "@mui/material";
import { To } from "react-router-dom";

import { ModalContents, ModalHeader, ModalWrapper } from "@atoms";
import { useModal } from "@context";
import type { WalletOption } from "@molecules";
import { WalletOptionButton } from "@molecules";
import { openInNewTab, getCip95Wallets } from "@utils";
import { useTranslation } from "@hooks";

type ChooseWalletModalState = {
  pathToNavigate?: To;
};

export const ChooseWalletModal = () => {
  const { t } = useTranslation();
  const { state } = useModal<ChooseWalletModalState>();

  const walletOptions: WalletOption[] = getCip95Wallets(window.cardano);

  return (
    <ModalWrapper dataTestId="connect-your-wallet-modal">
      <ModalHeader>{t("wallet.connectYourWallet")}</ModalHeader>
      <ModalContents>
        <Typography
          sx={{
            fontSize: "16px",
            fontWeight: "500",
            marginBottom: "24px",
            textAlign: "center",
          }}
        >
          {t("wallet.chooseWallet")}
        </Typography>
        <Box
          sx={{
            display: "flex",
            flexDirection: "column",
            maxHeight: "500px",
            overflow: "auto",
            width: "100%",
            padding: "8px",
          }}
        >
          {!walletOptions.length ? (
            <Typography
              color="primary"
              variant="body2"
              fontWeight={600}
              sx={{ textAlign: "center" }}
            >
              {t("wallet.noWalletsToConnect")}
            </Typography>
          ) : (
            walletOptions.map(({ icon, label, name, cip95Available }) => (
              <WalletOptionButton
                dataTestId={`${name}-wallet-button`}
                key={name}
                icon={icon}
                label={label}
                name={name}
                cip95Available={cip95Available}
                pathToNavigate={state?.pathToNavigate}
              />
            ))
          )}
        </Box>
        <Typography
          sx={{
            fontSize: "11px",
            fontWeight: "500",
            marginTop: "24px",
            textAlign: "center",
          }}
        >
          {t("wallet.cantSeeWalletQuestion")}
          <Link
            fontSize={11}
            fontWeight={500}
            onClick={() =>
              openInNewTab(
                "https://docs.sanchogov.tools/how-to-use-the-govtool/getting-started/get-a-compatible-wallet",
              )
            }
            sx={{ cursor: "pointer" }}
          >
            {t("here")}
          </Link>
          .
        </Typography>
      </ModalContents>
    </ModalWrapper>
  );
};
