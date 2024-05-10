"use client";
import React, {
  useEffect,
  useState,
  useContext,
  useCallback,
  useMemo,
} from "react";
import { loadSpace } from "@usersnap/browser";

const API_KEY = process.env.NEXT_PUBLIC_USERSNAP_SPACE_API_KEY;

export const UsersnapContext = React.createContext(null);

export const UsersnapProvider = ({ initParams, children }) => {
  const [usersnapApi, setUsersnapApi] = useState(null);

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
  const context = useContext(UsersnapContext);

  if (!context) {
    throw new Error("useUserSnapApi must be used within a UsersnapProvider");
  }

  return context;
}
