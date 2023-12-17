import MuiModal from "@mui/material/Modal";
import type { JSXElementConstructor, ReactElement } from "react";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type MuiModalChildren = ReactElement<
  any,
  string | JSXElementConstructor<any>
>;

interface Props {
  open: boolean;
  children: MuiModalChildren;
  handleClose?: () => void;
}

export function Modal({ open, children, handleClose }: Props) {
  return (
    <MuiModal open={open} onClose={handleClose} disableAutoFocus>
      <>{children}</>
    </MuiModal>
  );
}
