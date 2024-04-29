import { Typography } from "@mui/material";

import { Loader, ModalContents, ModalHeader, ModalWrapper } from "@atoms";
import { useModal } from "@context";
import { useScreenDimension } from "@/hooks";

export interface LoadingModalState {
  message: React.ReactNode;
  title: string;
  dataTestId: string;
}

export const LoadingModal = () => {
  const { state } = useModal<LoadingModalState>();
  const { isMobile } = useScreenDimension();

  return (
    <ModalWrapper
      dataTestId={state ? state.dataTestId : "loading-modal"}
      hideCloseButton
    >
      <Loader size={100} />
      <ModalHeader sx={{ marginTop: "34px", px: isMobile ? 0 : 3 }}>
        {state?.title}
      </ModalHeader>
      <ModalContents>
        <Typography
          textAlign="center"
          sx={{ fontSize: "16px", fontWeight: "400" }}
        >
          {state?.message}{" "}
        </Typography>
      </ModalContents>
    </ModalWrapper>
  );
};
