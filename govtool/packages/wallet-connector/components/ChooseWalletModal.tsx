import { useMemo } from "react";
import { Box, Link, Modal, Typography } from "@mui/material";
import CloseIcon from "@mui/icons-material/Close";

import { WalletButton } from "./WalletButton";
import { ChooseWalletModalProps } from "./types";

export const ChooseWalletModal = ({
  onClose,
  isOpen,
  isWalletEnableLoading,
  onClickShowSupportedWallets,
  onClickWalletButton,
}: ChooseWalletModalProps) => {
  const walletOptions = useMemo(() => {
    if (!window.cardano) return [];
    const wallets = Object.entries(window.cardano);

    const mappedWallets = wallets
      .filter(
        ([walletLabel, values]) =>
          walletLabel === values.name.toLowerCase() &&
          Boolean(
            values?.supportedExtensions?.find(
              (extension) => extension.cip === 95
            )
          )
      )
      .map(([walletLabel, values]) => ({
        icon: values.icon,
        label: walletLabel,
        name: values.name,
      }));

    return mappedWallets;
  }, [window]);

  return (
    <Modal onClose={onClose} open={isOpen}>
      <Box sx={style.container}>
        <CloseIcon
          data-testid="close-modal-button"
          onClick={onClose}
          sx={style.closeButton}
        />
        <Box>
          <Typography sx={style.header}>Connect your Wallet</Typography>
          <Typography sx={style.description}>
            Choose the wallet you want to connect with
          </Typography>
          <Box sx={style.buttonsContainer}>
            {!walletOptions.length ? (
              <Typography sx={style.emptyState}>
                You don't have wallets to connect, install a wallet and refresh
                the page and try again
              </Typography>
            ) : (
              walletOptions.map(({ icon, label, name }) => (
                <WalletButton
                  onClick={() => onClickWalletButton(name)}
                  isLoading={isWalletEnableLoading}
                  dataTestId={`${name}-wallet-button`}
                  key={name}
                  icon={icon}
                  label={label}
                  name={name}
                />
              ))
            )}
          </Box>
          {onClickShowSupportedWallets && (
            <Typography sx={style.information}>
              For more information please check the{" "}
              <Link onClick={onClickShowSupportedWallets} sx={style.link}>
                supported wallets list.
              </Link>
            </Typography>
          )}
        </Box>
      </Box>
    </Modal>
  );
};

const style = {
  buttonsContainer: { paddingX: { md: "42px" } },
  closeButton: {
    color: "#C1C2C1",
    cursor: "pointer",
    position: "absolute",
    right: 24,
    top: 24,
  },
  container: {
    background: "#fbfbff",
    borderRadius: "24px",
    boxShadow: "1px 2px 11px 0px #00123d5e",
    display: "flex",
    flexDirection: "column",
    left: "50%",
    maxHeight: "90vh",
    maxWidth: "462px",
    outline: "none",
    padding: "52px 24px 34px 24px",
    position: "absolute",
    top: "50%",
    transform: "translate(-50%, -50%)",
    width: "80vw",
  },
  description: {
    fontSize: 16,
    fontWeight: 400,
    lineHeight: "24px",
    marginBottom: "24px",
    textAlign: "center",
  },
  emptyState: {
    color: "primary",
    fontSize: 14,
    fontWeight: 600,
    textAlign: "center",
  },
  header: {
    fontSize: 28,
    fontWeight: 500,
    lineHeight: "36px",
    marginBottom: "8px",
    textAlign: "center",
  },
  information: {
    fontSize: "11px",
    fontWeight: 500,
    marginTop: "24px",
    textAlign: "center",
  },
  link: { cursor: "pointer", fontSize: 11, fontWeight: 500 },
};
