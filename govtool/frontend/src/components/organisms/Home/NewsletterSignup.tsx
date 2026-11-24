import { FormEvent, useId, useState } from "react";
import { Box, ButtonBase } from "@mui/material";
import { ArrowUpRight, Mail } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds, RADIUS } from "@/consts/designTokens";
import { LINKS } from "@/consts/links";
import { subscribeToNewsletter } from "@/services/requests/newsletter";
import { isEmailAddress } from "@/utils/newsletter";

import { DisplayHeading } from "./homeParts";

type Status = "idle" | "invalid" | "sending" | "sent" | "failed";

const ORANGE = ds.orange["400"];

/** Nested roof outlines and an isometric cube, as the design draws them. */
const SignupArt = () => (
  <Box
    aria-hidden
    component="svg"
    preserveAspectRatio="xMaxYMax meet"
    sx={{
      bottom: 0,
      display: { xxs: "none", md: "block" },
      height: "100%",
      position: "absolute",
      right: 0,
      width: "58%",
    }}
    viewBox="0 0 560 390"
  >
    {Array.from({ length: 16 }, (_, index) => {
      const inset = index * 11;
      const left = 0 + inset;
      const right = 560 - inset;
      const eave = 150 + inset * 0.55;
      const apex = 0 + inset * 1.2;
      return (
        <path
          key={index}
          d={`M${left} 390 V${eave} L${(left + right) / 2} ${apex} L${right} ${eave} V390`}
          fill="none"
          stroke={ORANGE}
          strokeWidth={1}
        />
      );
    })}
    {Array.from({ length: 9 }, (_, index) => (
      <path
        key={`cross-${index}`}
        d={`M${300 + index * 14} 0 L560 ${130 - index * 12}`}
        fill="none"
        stroke={ORANGE}
        strokeWidth={1}
      />
    ))}
    <g transform="translate(392 238)">
      <polygon fill="none" points="0,-68 59,-34 59,34 0,68 -59,34 -59,-34" stroke={ORANGE} strokeWidth={1} />
      <polygon fill={ds.orange["200"]} opacity={0.8} points="0,-34 29,-17 29,17 0,34 -29,17 -29,-17" />
      <path d="M0 -68 V0 M0 0 L59 34 M0 0 L-59 34 M-59 -34 L0 0 M59 -34 L0 0" fill="none" stroke={ORANGE} strokeWidth={1} />
    </g>
  </Box>
);

/**
 * The newsletter's sign-up: Ghost emails a confirmation link, and the reader
 * is subscribed once they follow it.
 */
export const NewsletterSignup = () => {
  const { t } = useTranslation();
  const inputId = useId();
  const [email, setEmail] = useState("");
  const [status, setStatus] = useState<Status>("idle");

  const submit = async (event: FormEvent) => {
    event.preventDefault();
    if (!isEmailAddress(email)) {
      setStatus("invalid");
      return;
    }
    setStatus("sending");
    try {
      await subscribeToNewsletter(email);
      setStatus("sent");
      setEmail("");
    } catch {
      setStatus("failed");
    }
  };

  const message = {
    idle: undefined,
    sending: undefined,
    invalid: t("landing.newsletter.invalid"),
    sent: t("landing.newsletter.sent"),
    failed: t("landing.newsletter.failed"),
  }[status];
  const isProblem = status === "invalid" || status === "failed";

  return (
    <Box
      data-testid="newsletter-signup"
      sx={{
        background: `linear-gradient(115deg, ${ds.primary["50"]} 0%, #F4F3F1 55%, #FBEDE3 100%)`,
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.xl}px`,
        minHeight: { md: 384 },
        overflow: "hidden",
        position: "relative",
      }}
    >
      <SignupArt />
      <Box
        component="form"
        noValidate
        onSubmit={submit}
        sx={{ maxWidth: 460, position: "relative", px: { xxs: 3, md: 6 }, py: { xxs: 4, md: 6 } }}
      >
        <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
          <Box
            aria-hidden
            sx={{ alignItems: "center", backgroundColor: ds.primary["100"], borderRadius: "50%", color: ds.primary["600"], display: "flex", height: 28, justifyContent: "center", width: 28 }}
          >
            <Mail size={14} />
          </Box>
          <Box component="p" sx={{ color: ds.primary["700"], fontSize: 11, fontWeight: 700, letterSpacing: "0.4px", m: 0, textTransform: "uppercase" }}>
            {t("landing.newsletter.eyebrow")}
          </Box>
        </Box>
        <DisplayHeading component="h3" size="sub" sx={{ mt: 3 }}>
          {t("landing.newsletter.title")}
        </DisplayHeading>
        <Box component="label" htmlFor={inputId} sx={{ color: ds.gray["900"], display: "block", fontSize: 14, fontWeight: 600, mt: 4 }}>
          {t("landing.newsletter.emailLabel")}
        </Box>
        <Box
          sx={{
            alignItems: "center",
            backgroundColor: "#FFFFFF",
            border: `1px solid ${isProblem ? ds.error["500"] : ds.gray["200"]}`,
            borderRadius: `${RADIUS.md}px`,
            display: "flex",
            gap: 1,
            mt: 1,
            p: 0.75,
            pl: 1.5,
            "&:focus-within": { borderColor: ds.primary["500"], boxShadow: `0 0 0 3px ${ds.primary["100"]}` },
          }}
        >
          <Mail aria-hidden color={ds.gray["500"]} size={16} />
          <Box
            aria-describedby={message ? `${inputId}-status` : undefined}
            aria-invalid={isProblem || undefined}
            autoComplete="email"
            component="input"
            data-testid="newsletter-email-input"
            id={inputId}
            onChange={(event: React.ChangeEvent<HTMLInputElement>) => {
              setEmail(event.target.value);
              if (status !== "sending") setStatus("idle");
            }}
            placeholder={t("landing.newsletter.placeholder")}
            sx={{ background: "none", border: "none", color: ds.gray["900"], flex: 1, fontFamily: "inherit", fontSize: 14, minWidth: 0, outline: "none", py: 1 }}
            type="email"
            value={email}
          />
          <ButtonBase
            data-testid="newsletter-subscribe-button"
            disabled={status === "sending"}
            sx={{
              backgroundColor: ds.primary["50"],
              borderRadius: `${RADIUS.sm}px`,
              color: ds.primary["700"],
              flexShrink: 0,
              fontFamily: "inherit",
              fontSize: 14,
              fontWeight: 600,
              gap: 0.5,
              height: 36,
              px: 1.5,
              "&:hover": { backgroundColor: ds.primary["100"] },
              "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
              "&.Mui-disabled": { opacity: 0.7 },
            }}
            type="submit"
          >
            {status === "sending" ? t("landing.newsletter.sending") : t("landing.newsletter.subscribe")}
            <ArrowUpRight aria-hidden size={14} />
          </ButtonBase>
        </Box>
        <Box
          aria-live="polite"
          data-testid="newsletter-status"
          id={`${inputId}-status`}
          role="status"
          sx={{ color: isProblem ? ds.error["700"] : ds.success["700"], fontSize: 13, fontWeight: 500, minHeight: 20, mt: 1 }}
        >
          {message}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 12, lineHeight: "18px", m: 0, mt: 0.5 }}>
          {`${t("landing.newsletter.legal")} `}
          <Box
            component="a"
            href={LINKS.PRIVACY_POLICY}
            rel="noopener noreferrer"
            sx={{ color: "inherit", textDecoration: "underline" }}
            target="_blank"
          >
            {t("landing.newsletter.privacyPolicy")}
          </Box>
          .
        </Box>
      </Box>
    </Box>
  );
};
