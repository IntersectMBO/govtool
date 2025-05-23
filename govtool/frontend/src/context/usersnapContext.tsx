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
    const initUsersnapSpace = async () => {
      if (API_KEY) {
        try {
          const api = await loadSpace(API_KEY);
          api.init({
            ...initParams,
            customFields: {
              sentiment_score: {
                type: 'rating',
                label: 'How would you rate your experience?',
                required: true,
                options: [1, 2, 3, 4, 5]
              },
              additional_notes: {
                type: 'textarea',
                label: 'Additional Notes',
                required: false
              }
            },
            feedbackTypes: [
              {
                id: 'bug',
                label: 'Report a Bug',
                description: 'Something is not working as expected'
              },
              {
                id: 'idea',
                label: 'Suggest a New Idea',
                description: 'Share your ideas for improvement'
              },
              {
                id: 'sentiment',
                label: 'Share Your Experience',
                description: 'Rate your experience and provide feedback'
              }
            ]
          });
          setUsersnapApi(api);
        } catch (error) {
          console.error(error);
        }
      }
    };
    initUsersnapSpace();
  }, [initParams, API_KEY]);

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