import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";
import { DataActionsBarProvider } from "./dataActionsBar";
import { FeatureFlagProvider } from "./featureFlag";
import { GovernanceActionProvider } from "./governanceAction";

interface Props {
  children: React.ReactNode;
}

const ContextProviders = ({ children }: Props) => (
  <GovernanceActionProvider>
    <FeatureFlagProvider>
      <ModalProvider>
        <SnackbarProvider>
          <DataActionsBarProvider>
            <CardanoProvider>{children}</CardanoProvider>
          </DataActionsBarProvider>
        </SnackbarProvider>
      </ModalProvider>
    </FeatureFlagProvider>
  </GovernanceActionProvider>
);

export { ContextProviders, useCardano, useModal, useSnackbar };
