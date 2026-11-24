import { SxProps, styled } from "@mui/material/styles";

import { X } from "lucide-react";
import { useModal } from "@context";
import { callAll } from "@utils";
import { forwardRef } from "react";
import { useTranslation } from "react-i18next";

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
        width: 80vw;
        max-width: 510px;
        padding: 40px 24px 32px 24px;
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

export const CloseButton = styled("button")`
  align-items: center;
  background: none;
  border: none;
  border-radius: 50%;
  color: #475467;
  cursor: pointer;
  display: flex;
  height: 36px;
  justify-content: center;
  padding: 0;
  position: absolute;
  right: 14px;
  top: 14px;
  width: 36px;
  &:hover {
    background: #f2f4f7;
  }
  &:focus-visible {
    outline: 2px solid #1447e6;
  }
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
    const { t } = useTranslation();

    return (
      <BaseWrapper variant={variant} data-testid={dataTestId} sx={sx} ref={ref}>
        {variant !== "popup" && !hideCloseButton && (
          <CloseButton
            aria-label={t("close")}
            data-testid="close-modal-button"
            onClick={callAll(closeModal, onClose)}
            type="button"
          >
            <X aria-hidden size={18} />
          </CloseButton>
        )}
        {children}
      </BaseWrapper>
    );
  },
);
