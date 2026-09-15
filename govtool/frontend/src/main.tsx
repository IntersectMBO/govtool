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

const { version } = pkg;

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      refetchOnWindowFocus: false,
      refetchOnMount: false,
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

Sentry.setTag("pdf_ui_version", pkg.dependencies["@intersect.mbo/pdf-ui"]);
Sentry.setTag(
  "govtool_outcomes_pillar_ui_version",
  pkg.dependencies["@intersect.mbo/govtool-outcomes-pillar-ui"],
);

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
