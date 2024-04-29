import { Typography } from "@mui/material";

import { Button } from "@atoms";
import { ICONS } from "@consts";
import { useModal } from "@context";

export const ExternalModalButton = ({
  label,
  url,
}: {
  label: string;
  url: string;
}) => {
  const { openModal } = useModal();

  return (
    <Button
      onClick={() => {
        openModal({
          type: "externalLink",
          state: {
            externalLink: url,
          },
        });
      }}
      sx={{
        p: 0,
        mb: 4,
        ":hover": {
          backgroundColor: "transparent",
        },
      }}
      disableRipple
      variant="text"
      data-testid="external-modal-button"
    >
      <Typography variant="body1" fontWeight={500} color="primary">
        {label}
      </Typography>
      <img
        alt="external link"
        src={ICONS.externalLinkIcon}
        height="20"
        width="20"
        style={{ marginLeft: "8px" }}
      />
    </Button>
  );
};
