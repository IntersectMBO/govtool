import { Button, Link, Typography } from "@mui/material";

import { ModalContents, ModalHeader, ModalWrapper } from "@atoms";
import { ICONS, IMAGES } from "@consts";
import { useModal } from "@context";
import { openInNewTab } from "@/utils";
import { useScreenDimension } from "@/hooks";
import { usei18n } from "@/translations";

export interface StatusModalState {
  buttonText?: string;
  status: "warning" | "info" | "success";
  isInfo?: boolean;
  link?: string;
  message: React.ReactNode;
  onSubmit?: () => void;
  title: string;
  dataTestId: string;
}

export function StatusModal() {
  const { state, closeModal } = useModal<StatusModalState>();
  const { isMobile } = useScreenDimension();
  const { t } = usei18n();

  return (
    <ModalWrapper dataTestId={state ? state.dataTestId : "status-modal"}>
      <img
        alt="Status icon"
        src={
          state?.status === "warning"
            ? IMAGES.warningImage
            : state?.status === "success"
            ? IMAGES.successImage
            : ICONS.timerIcon
        }
        style={{ height: "84px", margin: "0 auto", width: "84px" }}
      />
      <ModalHeader sx={{ marginTop: "34px", px: isMobile ? 0 : 3 }}>
        {state?.title}
      </ModalHeader>
      <ModalContents>
        <Typography
          textAlign="center"
          sx={{ fontSize: "16px", fontWeight: "400" }}
        >
          {state?.message}{" "}
          {state?.link && (
            <Link
              onClick={() => openInNewTab(state?.link || "")}
              target="_blank"
              sx={[{ "&:hover": { cursor: "pointer" } }]}
            >
              {t("thisLink")}
            </Link>
          )}
        </Typography>
      </ModalContents>
      <Button
        data-testid={"confirm-modal-button"}
        onClick={state?.onSubmit ? state?.onSubmit : closeModal}
        sx={{
          borderRadius: 50,
          margin: "0 auto",
          padding: "10px 26px",
          textTransform: "none",
          marginTop: "38px",
        }}
        variant="contained"
      >
        {state?.buttonText || t("confirm")}
      </Button>
    </ModalWrapper>
  );
}
