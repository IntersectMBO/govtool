import React, {
  useEffect,
  useState,
  useContext,
  useCallback,
  useMemo,
} from "react";
import { InitOptions, WidgetApi, loadSpace } from "@usersnap/browser";

type WidgetValues = {
  assignee?: string;
  custom?: object;
  labels?: Array<string>;
  visitor?: string;
};
type WidgetEventApi = {
  setValue: <K extends keyof WidgetValues>(
    key: K,
    value: WidgetValues[K],
  ) => void;
};

type WidgetBeforeSubmitEvent = {
  apiKey: string;
  api: WidgetEventApi;
  values: WidgetValues;
};

type WidgetOpenEvent = {
  api: WidgetEventApi;
  type: "open";
};

type SpaceEventCallback = (
  event: WidgetOpenEvent | WidgetBeforeSubmitEvent,
) => void;

type SpaceEventName = "open" | "close" | "beforeSubmit" | "submit";

const API_KEY = import.meta.env.VITE_USERSNAP_SPACE_API_KEY;

type UsersnapProviderProps = {
  initParams?: InitOptions;
  children?: React.ReactNode;
};

type UsersnapAPI = {
  init: (params?: InitOptions | undefined) => Promise<void>;
  logEvent: (eventName: string) => Promise<void>;
  show: (apiKey: string) => Promise<WidgetApi>;
  hide: (apiKey: string) => Promise<void>;
  destroy: () => Promise<void>;
  on: (eventName: SpaceEventName, callback: SpaceEventCallback) => void;
  off: (eventName: SpaceEventName, callback: SpaceEventCallback) => void;
};

const defaultValues = {
  openFeedbackWindow: () => {},
};

export const UsersnapContext = React.createContext(defaultValues);

export const UsersnapProvider = ({
  initParams,
  children,
}: UsersnapProviderProps) => {
  const [usersnapApi, setUsersnapApi] = useState<UsersnapAPI | null>(null);

  const openFeedbackWindow = useCallback(() => {
    if (usersnapApi) {
      usersnapApi.logEvent("open_feedback");
    }
  }, [usersnapApi]);

  useEffect(() => {
    loadSpace(API_KEY).then((api) => {
      api.init(initParams);
      setUsersnapApi(api);
    });
  }, [initParams]);

  const value = useMemo(() => ({ openFeedbackWindow }), [openFeedbackWindow]);

  return (
    <UsersnapContext.Provider value={value}>
      {children}
    </UsersnapContext.Provider>
  );
};

export function useUsersnapApi() {
  return useContext(UsersnapContext);
}
