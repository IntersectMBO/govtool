import type { Preview } from "@storybook/react";
import { ThemeProvider } from "@emotion/react";
import { theme } from "../src/theme";
import { MemoryRouter, Routes, Route } from "react-router-dom";
import { QueryClient, QueryClientProvider } from "react-query";

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
      </QueryClientProvider>
    ),
  ],
};

export default preview;
