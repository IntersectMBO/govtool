import type { SnackbarOrigin } from "@mui/material/Snackbar";
import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import { Snackbar, Alert } from "@mui/material";

import { SnackbarSeverity } from "@models";
import { useScreenDimension, useTranslation } from "@hooks";

interface ProviderProps {
  children: React.ReactNode;
}

interface SnackbarContextType {
  addSuccessAlert: (message: string, autoHideDuration?: number) => void;
  addErrorAlert: (message: string, autoHideDuration?: number) => void;
  addWarningAlert: (message: string, autoHideDuration?: number) => void;
  addChangesSavedAlert: () => void;
}

interface SnackbarMessage {
  key: number;
  message: string;
  severity: SnackbarSeverity;
  autoHideDuration: number;
}

interface State {
  open: boolean;
  messageInfo?: SnackbarMessage;
}

const SnackbarContext = createContext<SnackbarContextType>(
  {} as SnackbarContextType,
);
SnackbarContext.displayName = "SnackbarContext";

const DEFAULT_AUTO_HIDE_DURATION = 2000;
const defaultState: State = {
  open: false,
  messageInfo: undefined,
};
const defaultPosition = {
  vertical: "top",
  horizontal: "center",
} as SnackbarOrigin;

const SnackbarProvider = ({ children }: ProviderProps) => {
  const [snackPack, setSnackPack] = useState<readonly SnackbarMessage[]>([]);
  const [{ messageInfo, open }, setState] = useState(defaultState);
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const addWarningAlert = useCallback(
    (message: string, autoHideDuration = DEFAULT_AUTO_HIDE_DURATION) =>
      setSnackPack((prev) => [
        ...prev,
        {
          message,
          autoHideDuration,
          severity: "warning",
          key: new Date().getTime(),
        },
      ]),
    [],
  );

  const addSuccessAlert = useCallback(
    (message: string, autoHideDuration = DEFAULT_AUTO_HIDE_DURATION) =>
      setSnackPack((prev) => [
        ...prev,
        {
          message,
          autoHideDuration,
          severity: "success",
          key: new Date().getTime(),
        },
      ]),
    [],
  );

  const addErrorAlert = useCallback(
    (message: string, autoHideDuration = DEFAULT_AUTO_HIDE_DURATION) =>
      setSnackPack((prev) => [
        ...prev,
        {
          message,
          autoHideDuration,
          severity: "error",
          key: new Date().getTime(),
        },
      ]),
    [],
  );

  const addChangesSavedAlert = useCallback(
    () => addSuccessAlert(t("alerts.changesSaved")),
    [addSuccessAlert],
  );

  const value = useMemo(
    () => ({
      addSuccessAlert,
      addErrorAlert,
      addChangesSavedAlert,
      addWarningAlert,
    }),
    [addSuccessAlert, addErrorAlert, addChangesSavedAlert, addWarningAlert],
  );

  useEffect(() => {
    if (snackPack.length && !messageInfo) {
      // Set a new snack when we don't have an active one
      setState({ open: true, messageInfo: snackPack[0] });
      setSnackPack((prev) => prev.slice(1));
    } else if (snackPack.length && messageInfo && open) {
      // Close an active snack when a new one is added
      // setState((prev) => ({ ...prev, open: false }));
    }
  }, [snackPack, messageInfo, open]);

  const handleClose = (
    _event: React.SyntheticEvent | Event,
    reason?: string,
  ) => {
    if (reason === "clickaway") {
      return;
    }
    setState((prev) => ({ ...prev, open: false }));
  };

  const handleExited = () => {
    setState((prev) => ({ ...prev, messageInfo: undefined }));
  };

  const getBackgroundColor = (severity: SnackbarSeverity) => {
    if (severity === "success") {
      return "#62BC52";
    }
    if (severity === "error") {
      return "#FF3333";
    }
    return "#DEA029";
  };

  return (
    <SnackbarContext.Provider value={value}>
      {children}
      {messageInfo && (
        <Snackbar
          key={messageInfo.key}
          open={open}
          autoHideDuration={messageInfo.autoHideDuration}
          onClose={handleClose}
          TransitionProps={{ onExited: handleExited }}
          anchorOrigin={defaultPosition}
          sx={{ top: 48 }}
        >
          <Alert
            data-testid={`alert-${messageInfo.severity}`}
            onClose={handleClose}
            severity={messageInfo.severity}
            variant="filled"
            sx={{
              minWidth: isMobile ? "90%" : "30vw",
              backgroundColor: getBackgroundColor(messageInfo.severity),
            }}
          >
            {messageInfo.message}
          </Alert>
        </Snackbar>
      )}
    </SnackbarContext.Provider>
  );
};

function useSnackbar() {
  const context = useContext(SnackbarContext);
  if (context === undefined) {
    throw new Error("useSnackbar must be used within a SnackbarProvider");
  }
  return context;
}

export { SnackbarProvider, useSnackbar };
