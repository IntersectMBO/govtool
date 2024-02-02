import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";

interface Props {
  children: React.ReactNode;
}

const ContextProviders = ({ children }: Props) => {
  return (
    <SnackbarProvider>
      <CardanoProvider>
        <ModalProvider>{children}</ModalProvider>
      </CardanoProvider>
    </SnackbarProvider>
  );
};

export { ContextProviders, useCardano, useModal, useSnackbar };
