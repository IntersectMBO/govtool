import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";
import { DataActionsBarProvider } from "./dataActionsBar";
import { FeatureFlagProvider } from "./featureFlag";

interface Props {
  children: React.ReactNode;
}

const ContextProviders = ({ children }: Props) => (
  <FeatureFlagProvider>
    <ModalProvider>
      <SnackbarProvider>
        <DataActionsBarProvider>
          <CardanoProvider>{children}</CardanoProvider>
        </DataActionsBarProvider>
      </SnackbarProvider>
    </ModalProvider>
  </FeatureFlagProvider>
);

export { ContextProviders, useCardano, useModal, useSnackbar };
