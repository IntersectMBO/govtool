import { AppContextProvider } from "./appContext";
import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";
import { DataActionsBarProvider } from "./dataActionsBar";
import { FeatureFlagProvider } from "./featureFlag";
import { GovernanceActionProvider } from "./governanceAction";
import { AdaHandleProvider } from "./adaHandle";

interface Props {
  children: React.ReactNode;
}

const ContextProviders = ({ children }: Props) => (
  <AppContextProvider>
    <GovernanceActionProvider>
      <FeatureFlagProvider>
        <AdaHandleProvider>
          <ModalProvider>
            <SnackbarProvider>
              <DataActionsBarProvider>
                <CardanoProvider>{children}</CardanoProvider>
              </DataActionsBarProvider>
            </SnackbarProvider>
          </ModalProvider>
        </AdaHandleProvider>
      </FeatureFlagProvider>
    </GovernanceActionProvider>
  </AppContextProvider>
);

export { ContextProviders, useCardano, useModal, useSnackbar };
