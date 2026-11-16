import { forwardRef } from "react";
import { Box, Button, Link, Typography } from "@mui/material";
import { CircleCheck, Clock, TriangleAlert } from "lucide-react";

import { ModalContents, ModalHeader, ModalWrapper } from "@atoms";
import { useModal, useChatwoot } from "@context";
import { openInNewTab } from "@utils";
import { useScreenDimension, useTranslation } from "@hooks";
import { MetadataFailureDetails } from "@molecules";
import type { MetadataAnchor } from "@models";

export interface StatusModalState {
  buttonText?: string;
  cancelText?: string;
  feedbackText?: string;
  status: "warning" | "info" | "success";
  isInfo?: boolean;
  link?: string;
  linkText?: string;
  message: React.ReactNode;
  onSubmit?: () => void;
  onCancel?: () => void;
  onFeedback?: () => void;
  title: string;
  dataTestId: string;
  /**
   * The (url, hash) whose validation failed. When the backend has a fetch
   * report for it, the modal offers it so the author can see why (D117).
   */
  metadataAnchor?: MetadataAnchor;
  /** The failed check's own report, shown instead of the anchor's (D152). */
  metadataReportId?: string;
  /** Why the check could not run, when it could not. */
  metadataError?: string;
}

const STATUS_TONES = {
  success: { bg: "#ECFDF3", text: "#039855" },
  warning: { bg: "#FFFAEB", text: "#DC6803" },
  info: { bg: "#EEF3FF", text: "#1447E6" },
};

export const StatusModal = forwardRef<HTMLDivElement>((_, ref) => {
  const { state, closeModal } = useModal<StatusModalState>();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { openFeedbackWindow } = useChatwoot();

  const onClickFeedback = () => {
    openFeedbackWindow();
    closeModal();
  };

  return (
    <ModalWrapper
      dataTestId={state ? state.dataTestId : "status-modal"}
      ref={ref}
    >
      <Box
        aria-hidden
        sx={{
          alignItems: "center",
          alignSelf: "center",
          backgroundColor: STATUS_TONES[state?.status ?? "info"].bg,
          borderRadius: "50%",
          color: STATUS_TONES[state?.status ?? "info"].text,
          display: "flex",
          height: 56,
          justifyContent: "center",
          width: 56,
        }}
      >
        {state?.status === "success" ? (
          <CircleCheck size={28} />
        ) : state?.status === "warning" ? (
          <TriangleAlert size={28} />
        ) : (
          <Clock size={28} />
        )}
      </Box>
      <ModalHeader sx={{ marginTop: "20px", px: isMobile ? 0 : 3 }}>
        {state?.title}
      </ModalHeader>
      <ModalContents>
        <Typography
          textAlign="center"
          sx={{
            color: "#475467",
            fontSize: "15px",
            fontWeight: "400",
            lineHeight: "22px",
            overflow: "hidden",
            display: "-webkit-box",
            WebkitBoxOrient: "vertical",
            WebkitLineClamp: 8,
            wordBreak: "break-word",
            whiteSpace: "pre-line",
          }}
        >
          {typeof state?.message === "string" && state?.message}
          {state?.link && (
            <Link
              onClick={() => openInNewTab(state?.link || "")}
              target="_blank"
              sx={[{ "&:hover": { cursor: "pointer" } }]}
            >
              {state?.linkText || t("thisLink")}
            </Link>
          )}
        </Typography>
        {state?.metadataAnchor && (
          <Box sx={{ mt: 2, textAlign: "left" }}>
            <MetadataFailureDetails
              anchor={state.metadataAnchor}
              reportId={state.metadataReportId}
              error={state.metadataError}
            />
          </Box>
        )}
      </ModalContents>
      <Button
        data-testid="confirm-modal-button"
        onClick={state?.onSubmit ? state?.onSubmit : closeModal}
        sx={{
          borderRadius: 50,
          margin: "0 auto",
          padding: "10px 26px",
          textTransform: "none",
          marginTop: "38px",
          width: "100%",
        }}
        variant="contained"
      >
        {state?.buttonText || t("confirm")}
      </Button>
      {state?.cancelText && (
        <Button
          data-testid="cancel-modal-button"
          onClick={state?.onCancel ? state?.onCancel : closeModal}
          sx={{
            borderRadius: 50,
            margin: "0 auto",
            padding: "10px 26px",
            textTransform: "none",
            marginTop: "24px",
            width: "100%",
          }}
          variant="outlined"
        >
          {state?.cancelText}
        </Button>
      )}
      {state?.feedbackText && (
        <Button
          data-testid="feedback-button"
          onClick={state?.onFeedback ? state?.onFeedback : onClickFeedback}
          sx={{
            borderRadius: 50,
            margin: "0 auto",
            padding: "10px 26px",
            textTransform: "none",
            marginTop: "24px",
            width: "100%",
          }}
          variant="outlined"
        >
          {state?.feedbackText}
        </Button>
      )}
    </ModalWrapper>
  );
});
