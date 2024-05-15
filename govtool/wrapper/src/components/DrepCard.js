import { Box, Card, Typography } from "@mui/material";
import ContentCopyIcon from "@mui/icons-material/ContentCopy";

export const DrepCard = ({ address, sx }) => {
  const copyAddress = (e) => {
    navigator.clipboard.writeText(address);
    e.stopPropagation();
  };

  return (
    <Card elevation={0} sx={{ ...style.container, ...sx }}>
      <Box sx={style.leftContainer}>
        <Typography color="grays.400" variant="bodyMediumS">
          My dRep ID:
        </Typography>
        <Typography variant="bodyRegularM" sx={style.address}>
          {address}
        </Typography>
      </Box>
      <ContentCopyIcon color="primary" onClick={copyAddress} sx={style.icon} />
    </Card>
  );
};

const style = {
  address: {
    marginTop: "6px",
    overflow: "hidden",
    textOverflow: "ellipsis",
    width: "170px",
  },
  container: {
    backgroundColor: "arcticWhite.900",
    border: "1px solid lightBlue",
    borderRadius: "12px",
    display: "flex",
    padding: "12px",
  },
  icon: { cursor: "pointer", height: "18px", width: "18px" },
  leftContainer: {
    display: "flex",
    flexDirection: "column",
    flex: 1,
  },
};
