import { ThemeProvider } from "@emotion/react";
import type { Preview } from "@storybook/react";
import React from "react";
import { I18nextProvider } from "react-i18next";
import { QueryClient, QueryClientProvider } from "react-query";
import { MemoryRouter, Route, Routes } from "react-router-dom";

import { FeatureFlagProvider } from "../src/context/featureFlag";
import { AppContextProvider } from "../src/context/appContext";
import { ModalProvider } from "../src/context/modal";
import { CardanoProvider } from "../src/context/wallet";
import i18n from "../src/i18n";
import { theme } from "../src/theme";
import { AdaHandleProvider } from "../src/context/adaHandle";

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
        <AppContextProvider>
          <FeatureFlagProvider>
            <AdaHandleProvider>
              <ThemeProvider theme={theme}>
                <CardanoProvider>
                  <ModalProvider>
                    <I18nextProvider i18n={i18n}>
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
                    </I18nextProvider>
                  </ModalProvider>
                </CardanoProvider>
              </ThemeProvider>
            </AdaHandleProvider>
          </FeatureFlagProvider>
        </AppContextProvider>
      </QueryClientProvider>
    ),
  ],
};

export default preview;
