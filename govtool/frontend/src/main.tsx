import React from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter } from "react-router-dom";
import { QueryClient, QueryClientProvider } from "react-query";
import { ReactQueryDevtools } from "react-query/devtools";
import TagManager from "react-gtm-module";
import { ThemeProvider } from "@emotion/react";
import * as Sentry from "@sentry/react";

import { ContextProviders, UsersnapProvider } from "@context";

import App from "./App.tsx";
import { theme } from "./theme.ts";
import "./i18n";
import pkg from "../package.json";

const { version } = pkg;

const queryClient = new QueryClient();

const tagManagerArgs = {
  gtmId: import.meta.env.VITE_GTM_ID,
};

TagManager.initialize(tagManagerArgs);

if (import.meta.env.VITE_SENTRY_DSN) {
  Sentry.init({
    dsn: import.meta.env.VITE_SENTRY_DSN,
    environment: import.meta.env.VITE_APP_ENV,
    release: version,
    integrations: [
      Sentry.browserTracingIntegration(),
      Sentry.replayIntegration(),
    ],
    tracesSampleRate: 1.0,
    replaysSessionSampleRate: 0.1,
    replaysOnErrorSampleRate: 1.0,
    beforeSend(event) {
      window.dataLayer = window.dataLayer || [];
      window.dataLayer.push({
        event: "sentryEvent",
        sentryEventId: event?.event_id || "default_event_id",
        sentryErrorMessage:
          event?.exception?.values?.[0]?.value || "Unknown Error",
      });
      return event;
    },
  });
}

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <ThemeProvider theme={theme}>
        <UsersnapProvider>
          <BrowserRouter>
            <ContextProviders>
              <App />
            </ContextProviders>
          </BrowserRouter>
        </UsersnapProvider>
      </ThemeProvider>
      {import.meta.env.VITE_IS_DEV && (
        <ReactQueryDevtools initialIsOpen={false} />
      )}
    </QueryClientProvider>
  </React.StrictMode>,
);
