import React from "react";
import type { Preview } from "@storybook/react";
import { ThemeProvider } from "@emotion/react";
import { theme } from "../src/theme";
import { MemoryRouter, Routes, Route } from "react-router-dom";
import { QueryClient, QueryClientProvider } from "react-query";
import { I18nextProvider } from "react-i18next";
import i18n from "../src/i18n";
import { ModalProvider } from "../src/context/modal";

const queryClient = new QueryClient();

const preview: Preview = {
  parameters: {
    actions: { argTypesRegex: "^on[A-Z].*" },
    controls: {
      matchers: {
        color: /(background|color)$/i,
        date: /Date$/,
      },
    },
  },
  decorators: [
    (Story) => (
      <QueryClientProvider client={queryClient}>
        <ModalProvider>
          <I18nextProvider i18n={i18n}>
            <ThemeProvider theme={theme}>
              <MemoryRouter>
                <Routes>
                  <Route
                    path="/*"
                    element={
                      <div
                        style={{
                          margin: "0px",
                          padding: "0px",
                          position: "relative",
                        }}
                      >
                        <Story />
                      </div>
                    }
                  />
                </Routes>
              </MemoryRouter>
            </ThemeProvider>
          </I18nextProvider>
        </ModalProvider>
      </QueryClientProvider>
    ),
  ],
};

export default preview;
