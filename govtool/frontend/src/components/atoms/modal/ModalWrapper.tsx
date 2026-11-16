import { SxProps, styled } from "@mui/material/styles";

import { ICONS } from "@consts";
import { useModal } from "@context";
import { callAll } from "@utils";
import { forwardRef } from "react";

interface Props {
  variant?: "modal" | "popup";
  onClose?: () => void;
  hideCloseButton?: boolean;
  children: React.ReactNode;
  dataTestId?: string;
  sx?: SxProps;
}

export const BaseWrapper = styled("div")<Pick<Props, "variant">>`
  box-shadow: 0px 24px 48px -12px rgba(16, 24, 40, 0.18);
  border: 1px solid #e5e9f0;
  max-height: 90vh;
  position: fixed;
  top: 50%;
  left: 50%;
  display: flex;
  flex-direction: column;
  background: #ffffff;
  border-radius: 16px;
  transform: translate(-50%, -50%);

  ${({ variant }) => {
    if (variant === "modal") {
      return `
        width: calc(100vw - 32px);
        max-width: 520px;
        padding: 40px 32px 32px 32px;
        box-sizing: border-box;
        max-height: 80vh;
        overflow: auto;
      `;
    }
    if (variant === "popup") {
      return `
        width: 320px;
        height: 320px;
      `;
    }
  }}
`;

export const CloseButton = styled("img")`
  cursor: pointer;
  position: absolute;
  top: 20px;
  right: 20px;
  width: 20px;
  height: 20px;
  opacity: 0.7;
`;

export const ModalWrapper = forwardRef<HTMLDivElement, Props>(
  (
    {
      children,
      onClose,
      variant = "modal",
      hideCloseButton = false,
      dataTestId = "modal",
      sx,
    },
    ref,
  ) => {
    const { closeModal } = useModal();

    return (
      <BaseWrapper variant={variant} data-testid={dataTestId} sx={sx} ref={ref}>
        {variant !== "popup" && !hideCloseButton && (
          <CloseButton
            data-testid="close-modal-button"
            alt="close"
            onClick={callAll(closeModal, onClose)}
            src={ICONS.closeIcon}
          />
        )}
        {children}
      </BaseWrapper>
    );
  },
);
