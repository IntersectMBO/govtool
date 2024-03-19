import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";

interface Props {
  children: React.ReactNode;
}

function ContextProviders({ children }: Props) {
  return (
    <ModalProvider>
      <SnackbarProvider>
        <CardanoProvider>{children}</CardanoProvider>
      </SnackbarProvider>
    </ModalProvider>
  );
}

export {
  ContextProviders, useCardano, useModal, useSnackbar,
};
