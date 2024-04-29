import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";
import { DataActionsBarProvider } from "./dataActionsBar";

interface Props {
  children: React.ReactNode;
}

const ContextProviders = ({ children }: Props) => (
  <ModalProvider>
    <SnackbarProvider>
      <DataActionsBarProvider>
        <CardanoProvider>{children}</CardanoProvider>
      </DataActionsBarProvider>
    </SnackbarProvider>
  </ModalProvider>
);

export { ContextProviders, useCardano, useModal, useSnackbar };
