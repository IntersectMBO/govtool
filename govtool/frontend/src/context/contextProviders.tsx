import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";

interface Props {
  children: React.ReactNode;
}

const ContextProviders = ({ children }: Props) => (
  <ModalProvider>
    <SnackbarProvider>
      <CardanoProvider>{children}</CardanoProvider>
    </SnackbarProvider>
  </ModalProvider>
);

export {
  ContextProviders, useCardano, useModal, useSnackbar,
};
