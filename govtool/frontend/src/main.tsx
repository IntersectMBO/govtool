import React from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter } from "react-router";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { ReactQueryDevtools } from "@tanstack/react-query-devtools";
import { ThemeProvider } from "@emotion/react";
import * as Sentry from "@sentry/react";

import { ContextProviders, ChatwootProvider } from "@context";

import App from "./App.tsx";
import { theme } from "./theme.ts";
import "./i18n";
import pkg from "../package.json";
import { env } from "./config/env.ts";
import {
  SEARCH_NOT_READY_RETRIES,
  SEARCH_NOT_READY_RETRY_DELAY_MS,
  isSearchNotReady,
} from "./utils/searchNotReady.ts";

const { version } = pkg;

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: false,
      refetchOnMount: false,
      // A search the backend cannot run yet is asked again for about a
      // minute; anything else keeps react-query's defaults.
      retry: (failureCount, error) =>
        failureCount <
        (isSearchNotReady(error) ? SEARCH_NOT_READY_RETRIES : 3),
      retryDelay: (failureCount, error) => {
        if (isSearchNotReady(error)) return SEARCH_NOT_READY_RETRY_DELAY_MS;
        return Math.min(1000 * 2 ** failureCount, 30_000);
      },
    },
  },
});

if (env.VITE_SENTRY_DSN) {
  Sentry.init({
  dsn: env.VITE_SENTRY_DSN,
  environment: env.VITE_APP_ENV,
  release: version,
  integrations: [
    Sentry.browserTracingIntegration(),
    Sentry.replayIntegration(),
  ],
  tracesSampleRate: 1.0,
  replaysSessionSampleRate: 0.1,
  replaysOnErrorSampleRate: 1.0,
});
}

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <ThemeProvider theme={theme}>
        <ChatwootProvider>
          <BrowserRouter>
            <ContextProviders>
              <App />
            </ContextProviders>
          </BrowserRouter>
        </ChatwootProvider>
      </ThemeProvider>
      {env.VITE_IS_DEV && (
        <ReactQueryDevtools initialIsOpen={false} />
      )}
    </QueryClientProvider>
  </React.StrictMode>,
);
