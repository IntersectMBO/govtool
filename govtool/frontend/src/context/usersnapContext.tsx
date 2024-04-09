import React, { useEffect, useState, useContext, useCallback } from "react";
import { InitOptions, WidgetApi, loadSpace } from "@usersnap/browser";
import {
  SpaceEventCallback,
  SpaceEventName,
} from "node_modules/@usersnap/browser/dist/types";

const API_KEY = import.meta.env.VITE_USERSNAP_SPACE_API_KEY;

const defaultValues = {
  openFeedbackWindow: () => {},
};

export const UsersnapContext = React.createContext(defaultValues);

type API = {
  init: (params?: InitOptions | undefined) => Promise<void>;
  logEvent: (eventName: string) => Promise<void>;
  show: (apiKey: string) => Promise<WidgetApi>;
  hide: (apiKey: string) => Promise<void>;
  destroy: () => Promise<void>;
  on: (eventName: SpaceEventName, callback: SpaceEventCallback) => void;
  off: (eventName: SpaceEventName, callback: SpaceEventCallback) => void;
};

export const UsersnapProvider = ({
  initParams,
  children,
}: {
  initParams?: InitOptions;
  children?: React.ReactNode;
}) => {
  const [usersnapApi, setUsersnapApi] = useState<API | null>(null);

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

  return (
    <UsersnapContext.Provider value={{ openFeedbackWindow }}>
      {children}
    </UsersnapContext.Provider>
  );
};

export function useUsersnapApi() {
  return useContext(UsersnapContext);
}
