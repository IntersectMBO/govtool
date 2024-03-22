import React from "react";
import ReactDOM from "react-dom/client";
import {
  BrowserRouter,
  createRoutesFromChildren,
  matchRoutes,
  useLocation,
  useNavigationType,
} from "react-router-dom";
import { QueryClient, QueryClientProvider } from "react-query";
import TagManager from "react-gtm-module";
import { ThemeProvider } from "@emotion/react";
import * as Sentry from "@sentry/react";

import { ContextProviders } from "@context";

import App from "./App.tsx";
import { theme } from "./theme.ts";
import "./i18n";

const queryClient = new QueryClient();

interface SentryEventDataLayer {
  event: string;
  sentryEventId: string;
  sentryErrorMessage?: JSONValue;
}

// TODO: Move to types
declare global {
  interface Window {
    dataLayer: SentryEventDataLayer[];
  }
}

const tagManagerArgs = {
  gtmId: import.meta.env.VITE_GTM_ID,
};

TagManager.initialize(tagManagerArgs);

Sentry.init({
  dsn: import.meta.env.VITE_SENTRY_DSN,
  integrations: [
    new Sentry.BrowserTracing({
      routingInstrumentation: Sentry.reactRouterV6Instrumentation(
        React.useEffect,
        useLocation,
        useNavigationType,
        createRoutesFromChildren,
        matchRoutes,
      ),
    }),
    new Sentry.Replay(),
  ],

  tracesSampleRate: 1.0,

  replaysSessionSampleRate: 0.1,
  replaysOnErrorSampleRate: 1.0,
});

Sentry.addGlobalEventProcessor((event) => {
  window.dataLayer = window.dataLayer || [];

  const errorMessage =
    (event.exception &&
      event.exception.values &&
      event.exception.values[0] &&
      event.exception.values[0].value) ||
    "Unknown Error";

  window.dataLayer.push({
    event: "sentryEvent",
    sentryEventId: event.event_id || "default_event_id",
    sentryErrorMessage: errorMessage,
  });

  return event;
});

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <ThemeProvider theme={theme}>
        <BrowserRouter>
          <ContextProviders>
            <App />
          </ContextProviders>
        </BrowserRouter>
      </ThemeProvider>
    </QueryClientProvider>
  </React.StrictMode>,
);
