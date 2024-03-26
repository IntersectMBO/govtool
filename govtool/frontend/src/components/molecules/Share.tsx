import { useState } from "react";
import { Box, Popover } from "@mui/material";

import { Typography } from "@atoms";
import { ICONS } from "@consts";
import { useSnackbar } from "@context";
import { useTranslation } from "@hooks";

export const Share = ({ link }: { link: string }) => {
  const { addSuccessAlert } = useSnackbar();
  const { t } = useTranslation();
  const [anchorEl, setAnchorEl] = useState<HTMLDivElement | null>(null);

  const handleClick = (e: React.MouseEvent<HTMLDivElement>) => {
    setAnchorEl(e.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const onCopy = (e: React.MouseEvent<HTMLDivElement>) => {
    navigator.clipboard.writeText(link);
    addSuccessAlert(t("alerts.copiedToClipboard"));
    e.stopPropagation();
  };

  const open = Boolean(anchorEl);
  const id = open ? "simple-popover" : undefined;

  return (
    <>
      <Box
        aria-describedby={id}
        onClick={handleClick}
        sx={{
          alignItems: "center",
          bgcolor: "#F7F9FB",
          borderRadius: 50,
          boxShadow: "2px 2px 15px 0px #2F62DC47",
          cursor: "pointer",
          display: "flex",
          justifyContent: "center",
          padding: 1.5,
        }}
      >
        <img height={24} width={24} src={ICONS.share} alt="share icon" />
      </Box>
      <Popover
        id={id}
        open={open}
        onClose={handleClose}
        anchorEl={anchorEl}
        marginThreshold={12}
        sx={{ botderRadius: 12 }}
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
            display: "flex",
            flexDirection: "column",
            justifyContent: "center",
            padding: "12px 24px",
          }}
        >
          <Typography sx={{ alignSelf: "flex-start" }}>{t("share")}</Typography>
          <Box
            onClick={onCopy}
            sx={{
              alignItems: "center",
              bgcolor: "lightBlue",
              borderRadius: 50,
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
          </Box>
          <Typography variant="caption">{t("clickToCopyLink")}</Typography>
        </Box>
      </Popover>
    </>
  );
};
