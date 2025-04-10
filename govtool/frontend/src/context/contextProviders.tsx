import { AppContextProvider } from "./appContext";
import { CardanoProvider, useCardano } from "./wallet";
import { ModalProvider, useModal } from "./modal";
import { SnackbarProvider, useSnackbar } from "./snackbar";
import { DataActionsBarProvider } from "./dataActionsBar";
import { FeatureFlagProvider } from "./featureFlag";
import { GovernanceActionProvider } from "./governanceAction";
import { AdaHandleProvider } from "./adaHandle";
import { ProposalDiscussionProvider } from "./proposalDiscussion";

interface Props {
  children: React.ReactNode;
}

const ContextProviders = ({ children }: Props) => (
  <AppContextProvider>
    <GovernanceActionProvider>
      <ProposalDiscussionProvider>
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
      </ProposalDiscussionProvider>
    </GovernanceActionProvider>
  </AppContextProvider>
);

export { ContextProviders, useCardano, useModal, useSnackbar };
