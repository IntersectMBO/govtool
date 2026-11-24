import { Box } from "@mui/material";

import { useTranslation } from "@hooks";
import { ds } from "@/consts/designTokens";

export type FlowStepperProps = {
  /** The flow's name, e.g. "DRep registration". */
  title: string;
  steps: string[];
  /** 1-based; steps up to and including it are filled. */
  current: number;
};

/** A flow's progress: its name, "Step N of M" and one segment per step. */
export const FlowStepper = ({ title, steps, current }: FlowStepperProps) => {
  const { t } = useTranslation();
  const step = Math.min(Math.max(current, 1), steps.length);

  return (
    <Box
      aria-label={t("flow.progress", { title, step, count: steps.length })}
      data-testid="flow-stepper"
      role="group"
      sx={{ display: "flex", flexDirection: "column", gap: 1.25 }}
    >
      <Box sx={{ alignItems: "baseline", display: "flex", gap: 2, justifyContent: "space-between" }}>
        <Box component="span" sx={{ color: ds.gray["700"], fontSize: 13, fontWeight: 600 }}>
          {title}
        </Box>
        <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, whiteSpace: "nowrap" }}>
          {t("flow.stepOf", { step, count: steps.length })}
        </Box>
      </Box>
      <Box
        component="ol"
        sx={{
          display: "grid",
          gap: 0.75,
          gridTemplateColumns: `repeat(${steps.length}, minmax(0, 1fr))`,
          listStyle: "none",
          m: 0,
          p: 0,
        }}
      >
        {steps.map((label, index) => {
          const isDone = index < step;
          return (
            <Box
              key={label}
              aria-current={index === step - 1 ? "step" : undefined}
              component="li"
              sx={{ display: "flex", flexDirection: "column", gap: 0.75, minWidth: 0 }}
            >
              <Box
                sx={{
                  backgroundColor: isDone ? ds.primary["600"] : ds.gray["200"],
                  borderRadius: "2px",
                  height: 4,
                }}
              />
              <Box
                component="span"
                sx={{
                  color: index === step - 1 ? ds.gray["900"] : ds.gray["600"],
                  display: { xxs: index === step - 1 ? "block" : "none", md: "block" },
                  fontSize: 12,
                  fontWeight: index === step - 1 ? 600 : 400,
                  overflow: "hidden",
                  textOverflow: "ellipsis",
                  whiteSpace: "nowrap",
                }}
              >
                {label}
              </Box>
            </Box>
          );
        })}
      </Box>
    </Box>
  );
};
