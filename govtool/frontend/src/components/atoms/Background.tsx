import { ReactNode } from "react";
import { Box } from "@mui/material";

type BackgroundProps = {
  children: ReactNode;
};

/**
 * The redesign's page background: the warm off-white body (index.html) with a
 * soft blue glow behind the header and page title, as every design frame has.
 */
export const Background = ({ children }: BackgroundProps) => (
  <>
    <Box
      aria-hidden
      data-testid="page-glow"
      sx={{
        background:
          "radial-gradient(ellipse 70% 100% at 50% 0%, #E7EFFF 0%, rgba(252, 252, 250, 0) 100%)",
        height: 320,
        left: 0,
        pointerEvents: "none",
        position: "absolute",
        right: 0,
        top: 0,
        zIndex: -1,
      }}
    />
    {children}
  </>
);
