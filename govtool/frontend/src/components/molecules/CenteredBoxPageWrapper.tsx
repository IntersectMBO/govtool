import { FC, PropsWithChildren } from "react";
import { Box } from "@mui/material";
import { useNavigate } from "react-router";
import { ChevronLeft } from "lucide-react";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { Footer, TopNav } from "@organisms";
import { useTranslation } from "@hooks";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

import { FlowStepper, FlowStepperProps } from "./FlowStepper";

interface Props {
  /** The flow's name: the stepper's title, or a label above the card. */
  pageTitle: string;
  onClickBackToDashboard?: () => void;
  /** Unused since the shared top nav shows voting power itself. */
  showVotingPower?: boolean;
  /** Centre the content without the card, for loaders. */
  hideBox?: boolean;
  /** Steps that lay out their own section cards (create governance action). */
  frameless?: boolean;
  /** The flow's progress above the card, titled with `pageTitle`. */
  stepper?: Omit<FlowStepperProps, "title">;
}

/**
 * The frame of every wallet flow (registration, retirement, create governance
 * action): the shared top nav, a back link, the flow's stepper and its card.
 */
export const CenteredBoxPageWrapper: FC<PropsWithChildren<Props>> = ({
  pageTitle,
  onClickBackToDashboard,
  hideBox,
  frameless,
  stepper,
  children,
}) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  return (
    <Background>
      <Box display="flex" minHeight="100vh" flexDirection="column">
        <TopNav onNavigateAway={onClickBackToDashboard} />
        <Box
          component="main"
          sx={{
            boxSizing: "border-box",
            display: "flex",
            flex: 1,
            flexDirection: "column",
            gap: 3,
            maxWidth: 812,
            mx: "auto",
            px: { xxs: 2, md: 2 },
            py: { xxs: 3, md: 5 },
            width: "100%",
          }}
        >
          <Box
            component="button"
            // The id LinkWithIcon gave this link before.
            data-testid={`${t("backToDashboard").split(" ").join("-")}-link`}
            onClick={onClickBackToDashboard ?? (() => navigate(PATHS.dashboard))}
            type="button"
            sx={{
              alignItems: "center",
              alignSelf: "flex-start",
              background: "none",
              border: "none",
              color: ds.gray["600"],
              cursor: "pointer",
              display: "flex",
              fontFamily: "inherit",
              fontSize: 13,
              gap: 0.5,
              p: 0,
              "&:hover": { color: ds.primary["600"] },
              "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
            }}
          >
            <ChevronLeft aria-hidden size={16} />
            {t("backToDashboard")}
          </Box>
          {stepper ? (
            <FlowStepper {...stepper} title={pageTitle} />
          ) : (
            !hideBox && (
              <Box
                component="p"
                data-testid="flow-title"
                sx={{ color: ds.gray["700"], fontSize: 13, fontWeight: 600, m: 0 }}
              >
                {pageTitle}
              </Box>
            )
          )}
          {frameless && (
            <Box sx={{ display: "flex", flexDirection: "column", gap: 3, width: "100%" }}>
              {children}
            </Box>
          )}
          {!frameless && hideBox && (
            <Box
              sx={{
                alignItems: "center",
                display: "flex",
                flex: 1,
                flexDirection: "column",
                justifyContent: "center",
              }}
            >
              {children}
            </Box>
          )}
          {!frameless && !hideBox && (
            <Box
              sx={{
                backgroundColor: "#FFFFFF",
                border: `1px solid ${ds.gray["200"]}`,
                borderRadius: `${RADIUS.xl}px`,
                boxShadow: SHADOWS.card,
                boxSizing: "border-box",
                display: "flex",
                flexDirection: "column",
                px: { xxs: 2.5, md: 5 },
                py: { xxs: 3, md: 5 },
                width: "100%",
              }}
            >
              {children}
            </Box>
          )}
        </Box>
        <Footer />
      </Box>
    </Background>
  );
};
