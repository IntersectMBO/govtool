import { MouseEvent, useState } from "react";
import { Box, ButtonBase, Popover } from "@mui/material";

import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { useSnackbar } from "@context";
import { useTranslation } from "@hooks";

export const Share = ({ link }: { link: string }) => {
  const { addSuccessAlert } = useSnackbar();
  const { t } = useTranslation();
  const [anchorEl, setAnchorEl] = useState<HTMLButtonElement | null>(null);
  const [isActive, setIsActive] = useState<boolean>(true);

  const handleClick = (event: MouseEvent<HTMLButtonElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const onCopy = (event: MouseEvent<HTMLButtonElement>) => {
    navigator.clipboard.writeText(link);
    addSuccessAlert(t("alerts.copiedToClipboard"));
    setIsActive(false);
    event.stopPropagation();
  };

  const open = Boolean(anchorEl);
  const id = open ? "simple-popover" : undefined;

  return (
    <>
      <ButtonBase
        data-testid="share-button"
        aria-describedby={id}
        onClick={handleClick}
        sx={(theme) => ({
          alignItems: "center",
          bgcolor: open ? "#F7F9FB" : "transparent",
          borderRadius: 50,
          boxShadow: open ? theme.shadows[1] : "none",
          cursor: "pointer",
          display: "flex",
          justifyContent: "center",
          padding: 1.5,
          transition: "all 0.3s",
          "&:hover": {
            boxShadow: theme.shadows[1],
            bgcolor: "#F7F9FB",
          },
        })}
      >
        <img alt="" height={24} width={24} src={ICONS.share} />
      </ButtonBase>
      <Popover
        id={id}
        open={open}
        onClose={handleClose}
        anchorEl={anchorEl}
        marginThreshold={12}
        anchorOrigin={{
          vertical: "bottom",
          horizontal: "right",
        }}
        transformOrigin={{
          vertical: "top",
          horizontal: "right",
        }}
      >
        <Box
          sx={{
            alignItems: "center",
            boxSizing: "border-box",
            display: "flex",
            flexDirection: "column",
            justifyContent: "center",
            padding: "12px 24px",
            px: 3,
            width: 148,
          }}
        >
          <Typography sx={{ alignSelf: "flex-start" }}>{t("share")}</Typography>
          <ButtonBase
            data-testid="copy-link-from-share-button"
            onClick={onCopy}
            sx={{
              alignItems: "center",
              bgcolor: isActive ? "lightBlue" : "neutralWhite",
              borderRadius: 50,
              boxShadow: (theme) => theme.shadows[1],
              cursor: "pointer",
              display: "flex",
              height: 48,
              justifyContent: "center",
              width: 48,
              mt: 1.5,
              mb: 1,
            }}
          >
            <img alt="link" height={24} src={ICONS.link} width={24} />
          </ButtonBase>
          <Typography variant="caption">
            {isActive ? t("clickToCopyLink") : t("linkCopied")}
          </Typography>
        </Box>
      </Popover>
    </>
  );
};
