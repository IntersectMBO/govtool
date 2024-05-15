import { Box, Button, Card, Typography, Chip } from "@mui/material";

export const WalletCard = ({ address, onClickDisconnect, network, sx }) => {
  return (
    <Card elevation={0} sx={{ ...style.container, ...sx }}>
      {network && (
        <Chip label={network} size="small" sx={style.chip} variant="info" />
      )}
      <Typography color="grays.400" variant="bodyMediumS">
        Connected Wallet:
      </Typography>
      <Box sx={style.bottomContainer}>
        <Typography sx={style.address} variant="bodyRegularM">
          {address}
        </Typography>
        {onClickDisconnect && (
          <Button
            data-testid="disconnect-button"
            onClick={onClickDisconnect}
            variant="link"
          >
            Disconnect
          </Button>
        )}
      </Box>
    </Card>
  );
};

const style = {
  address: {
    flex: 1,
    overflow: "hidden",
    textOverflow: "ellipsis",
    width: 10,
    mr: "6px",
  },
  bottomContainer: { alignItems: "center", display: "flex", mt: "8px" },
  chip: {
    position: "absolute",
    top: -12,
    right: 14,
  },
  container: {
    border: "1px solid lightBlue",
    borderRadius: "12px",
    marginTop: "32px",
    overflow: "visible",
    paddingY: "12px",
    paddingX: "13.5px",
    position: "relative",
  },
};
