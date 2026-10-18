import { useEffect, useState } from "react";
import { Box, Typography } from "@mui/material";
import { keyframes } from "@emotion/react";
import ConstructionRoundedIcon from "@mui/icons-material/ConstructionRounded";
import { Trans, useTranslation } from "react-i18next";
import { useCountdown } from "@hooks";
import { orange, primaryBlue, progressYellow } from "@/consts";
import { theme } from "@/theme";

/** Below this, the banner drops the duration and says it will be back soon. */
const FINAL_STRETCH_SECONDS = 5 * 60;

const swing = keyframes`
  0%, 60%, 100% { transform: rotate(0deg); }
  70% { transform: rotate(-16deg); }
  80% { transform: rotate(10deg); }
  90% { transform: rotate(-4deg); }
`;

const sheen = keyframes`
  0% { transform: translateX(-100%); }
  60%, 100% { transform: translateX(100%); }
`;

const tick = keyframes`
  from { transform: translateY(-45%); opacity: 0; }
  to { transform: translateY(0); opacity: 1; }
`;

const bounce = keyframes`
  0%, 60%, 100% { transform: translateY(0); opacity: 0.35; }
  30% { transform: translateY(-3px); opacity: 1; }
`;

const noMotion = {
  "@media (prefers-reduced-motion: reduce)": { animation: "none" },
};

const secondsUntil = (time: number) =>
  Math.max(0, Math.ceil((time - Date.now()) / 1000));

/** "about 1 hour", "about 25 minutes": rounded, since it is an estimate. */
const roundMinutes = (totalSeconds: number) => {
  const minutes = Math.ceil(totalSeconds / 60);
  return minutes >= 15 ? Math.round(minutes / 5) * 5 : minutes;
};

const toParts = (totalSeconds: number) => ({
  hours: Math.floor(totalSeconds / 3600),
  minutes: Math.floor((totalSeconds % 3600) / 60),
  seconds: totalSeconds % 60,
});

const AnimatedDots = () => (
  <Box
    component="span"
    aria-hidden
    data-testid="system-maintenance-dots"
    sx={{ display: "inline-flex", ml: 0.25 }}
  >
    {[0, 1, 2].map((dot) => (
      <Box
        key={dot}
        component="span"
        sx={{
          display: "inline-block",
          animation: `${bounce} 1.2s ease-in-out ${dot * 0.15}s infinite`,
          ...noMotion,
        }}
      >
        .
      </Box>
    ))}
  </Box>
);

const TimeTile = ({ value, unit }: { value: number; unit: string }) => (
  <Box
    sx={{
      display: "inline-flex",
      alignItems: "baseline",
      justifyContent: "center",
      gap: 0.25,
      px: 1,
      py: 0.5,
      minWidth: 44,
      fontFamily: theme.typography.fontFamily,
      borderRadius: 2.5,
      backgroundColor: "rgba(255, 255, 255, 0.8)",
      border: `1px solid ${theme.palette.neutralWhite}`,
      boxShadow: theme.shadows[1],
      overflow: "hidden",
    }}
  >
    <Box
      key={value}
      component="span"
      sx={{
        display: "inline-block",
        fontSize: { xxs: 15, md: 17 },
        fontWeight: 600,
        color: primaryBlue.c500,
        fontVariantNumeric: "tabular-nums",
        animation: `${tick} 0.35s ease-out`,
        ...noMotion,
      }}
    >
      {String(value).padStart(2, "0")}
    </Box>
    <Box
      component="span"
      sx={{ fontSize: 11, fontWeight: 500, color: theme.palette.fadedPurple }}
    >
      {unit}
    </Box>
  </Box>
);

type Props = {
  /** When maintenance is expected to end; without it there is no countdown. */
  endsAt?: number;
  /** When it started, which sizes the progress line. */
  startsAt?: number;
};

export const MaintenanceBanner = ({ endsAt, startsAt }: Props) => {
  const { t } = useTranslation();
  const { remaining, start } = useCountdown();
  // The end the countdown is running for; it restarts when the notice moves.
  const [countingTo, setCountingTo] = useState<number>();

  useEffect(() => {
    if (endsAt === undefined) return;
    start((endsAt - Date.now()) / 1000);
    setCountingTo(endsAt);
  }, [start, endsAt]);

  const hasTimer = endsAt !== undefined;
  const secondsLeft = hasTimer
    ? countingTo === endsAt
      ? remaining
      : secondsUntil(endsAt)
    : 0;
  const hasEnded = hasTimer && secondsLeft === 0;
  const isFinalStretch =
    hasTimer && (hasEnded || secondsLeft <= FINAL_STRETCH_SECONDS);

  const totalSeconds =
    hasTimer && startsAt !== undefined && startsAt < endsAt
      ? (endsAt - startsAt) / 1000
      : 0;
  const progress =
    totalSeconds > 0 ? Math.min(100, (secondsLeft / totalSeconds) * 100) : 0;
  const { hours, minutes, seconds } = toParts(secondsLeft);

  const minutesLeft = roundMinutes(secondsLeft);
  const duration =
    minutesLeft >= 60
      ? t("system.maintenance.hours", { count: Math.round(minutesLeft / 60) })
      : t("system.maintenance.minutes", { count: minutesLeft });

  let title = t("system.maintenance.title");
  if (hasEnded) title = t("system.maintenance.titleEnded");
  else if (isFinalStretch) title = t("system.maintenance.titleFinalStretch");

  return (
    <Box
      role="region"
      aria-label={t("system.maintenance.label")}
      data-testid="system-maintenance-banner"
      sx={{
        position: "relative",
        overflow: "hidden",
        background: `linear-gradient(100deg, ${progressYellow.c100} 0%, ${theme.palette.accentYellow} 50%, ${orange.c100} 100%)`,
        borderBottom: `1px solid ${progressYellow.c300}`,
      }}
    >
      {/* Soft light sweep */}
      <Box
        aria-hidden
        sx={{
          position: "absolute",
          inset: 0,
          background:
            "linear-gradient(100deg, transparent 30%, rgba(255, 255, 255, 0.45) 50%, transparent 70%)",
          animation: `${sheen} 7s ease-in-out infinite`,
          ...noMotion,
        }}
      />

      <Box
        sx={{
          position: "relative",
          maxWidth: 1290,
          mx: "auto",
          px: { xxs: 2, md: 5 },
          py: { xxs: 1.25, md: 1.5 },
          display: "flex",
          flexWrap: "wrap",
          alignItems: "center",
          columnGap: { xxs: 1.5, md: 2 },
          rowGap: 1,
        }}
      >
        <Box
          aria-hidden
          sx={{
            flexShrink: 0,
            width: { xxs: 32, md: 40 },
            height: { xxs: 32, md: 40 },
            borderRadius: "50%",
            display: "flex",
            alignItems: "center",
            justifyContent: "center",
            backgroundColor: theme.palette.neutralWhite,
            boxShadow: theme.shadows[2],
          }}
        >
          <ConstructionRoundedIcon
            sx={{
              fontSize: { xxs: 18, md: 22 },
              color: orange.c600,
              transformOrigin: "30% 70%",
              animation: `${swing} 2.8s ease-in-out infinite`,
              ...noMotion,
            }}
          />
        </Box>

        <Box sx={{ flex: "1 1 0", minWidth: 0 }}>
          <Typography
            component="p"
            sx={{
              fontSize: { xxs: 14, md: 15 },
              fontWeight: 600,
              lineHeight: 1.4,
            }}
          >
            {title}
            {isFinalStretch && !hasEnded && <AnimatedDots />}
          </Typography>
          {(!isFinalStretch || hasEnded) && (
            <Typography
              component="p"
              sx={{
                fontSize: { xxs: 12, md: 13 },
                lineHeight: 1.5,
                color: orange.c700,
              }}
            >
              <Trans
                i18nKey={
                  hasTimer && !hasEnded
                    ? "system.maintenance.description"
                    : "system.maintenance.descriptionNoEnd"
                }
                values={{ duration }}
                components={{
                  strong: (
                    <Box key="strong" component="strong" fontWeight={600} />
                  ),
                }}
              />
            </Typography>
          )}
        </Box>

        {hasTimer && (
          <Box
            aria-live="off"
            data-testid="system-maintenance-timer"
            sx={{
              display: "flex",
              alignItems: "center",
              gap: 1,
              flexBasis: { xxs: "100%", md: "auto" },
              pl: { xxs: 5.5, md: 0 },
              ml: { md: "auto" },
            }}
          >
            {hasEnded ? (
              <Box
                data-testid="system-maintenance-finishing"
                sx={{
                  display: "inline-flex",
                  alignItems: "baseline",
                  px: 1.5,
                  py: 0.75,
                  borderRadius: 2.5,
                  fontFamily: theme.typography.fontFamily,
                  fontSize: { xxs: 13, md: 14 },
                  fontWeight: 600,
                  color: primaryBlue.c500,
                  backgroundColor: "rgba(255, 255, 255, 0.8)",
                  border: `1px solid ${theme.palette.neutralWhite}`,
                  boxShadow: theme.shadows[1],
                }}
              >
                {t("system.maintenance.finishingUp")}
                <AnimatedDots />
              </Box>
            ) : (
              <Typography
                sx={{
                  fontSize: 11,
                  fontWeight: 600,
                  letterSpacing: "0.08em",
                  textTransform: "uppercase",
                  color: orange.c700,
                }}
              >
                {t("system.maintenance.backIn")}
              </Typography>
            )}
            {!hasEnded && (
              <Box sx={{ display: "flex", gap: 0.75 }}>
                {hours > 0 && (
                  <TimeTile value={hours} unit={t("system.maintenance.h")} />
                )}
                <TimeTile value={minutes} unit={t("system.maintenance.m")} />
                <TimeTile value={seconds} unit={t("system.maintenance.s")} />
              </Box>
            )}
          </Box>
        )}
      </Box>

      {/* Time remaining */}
      {progress > 0 && (
        <Box
          aria-hidden
          sx={{
            position: "absolute",
            left: 0,
            bottom: 0,
            height: 3,
            width: `${progress}%`,
            borderRadius: "0 3px 3px 0",
            background: `linear-gradient(90deg, ${progressYellow.c500}, ${orange.c400})`,
            transition: "width 1s linear",
          }}
        />
      )}
    </Box>
  );
};
