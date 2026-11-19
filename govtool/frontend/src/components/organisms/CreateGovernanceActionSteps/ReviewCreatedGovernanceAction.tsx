import { Dispatch, Fragment, SetStateAction } from "react";
import { Box } from "@mui/material";

import { GOVERNANCE_ACTION_FIELDS } from "@consts";
import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";
import { openInNewTab, testIdFromLabel } from "@utils";
import { ds, FONT_FAMILY, RADIUS, SHADOWS } from "@/consts/designTokens";

type ReviewCreatedGovernanceActionProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

const ABOUT_FIELDS = ["abstract", "motivation", "rationale"];

/** The test id the review has always used: from the form key, not the copy. */
const contentTestId = (key: string) =>
  `governance-action-${testIdFromLabel(
    key.charAt(0).toUpperCase() + key.slice(1).replace(/_/g, " "),
  )}-content`;

const sectionTitle = {
  color: ds.gray["900"],
  fontSize: 18,
  fontWeight: 600,
  m: 0,
};
const divider = { borderTop: `1px solid ${ds.gray["200"]}`, my: 3 };

export const ReviewCreatedGovernanceAction = ({
  setStep,
}: ReviewCreatedGovernanceActionProps) => {
  const { t } = useTranslation();
  const { getValues } = useCreateGovernanceActionForm();
  const values = getValues();
  const type = values.governance_action_type;
  const fields = (type
    ? GOVERNANCE_ACTION_FIELDS[type as keyof typeof GOVERNANCE_ACTION_FIELDS]
    : {}) as Record<string, { labelI18nKey: string }>;

  const value = (key: string) => (values as Record<string, unknown>)[key];
  const labelOf = (key: string) => (fields[key] ? t(fields[key].labelI18nKey) : key);
  const detailKeys = Object.keys(fields).filter(
    (key) => key !== "title" && !ABOUT_FIELDS.includes(key) && value(key),
  );
  const links = (values.references ?? [])
    .map((item, position) => ({ uri: item.uri, id: `${position}-${item.uri}` }))
    .filter((item) => item.uri);
  const isTreasury = type === "TreasuryWithdrawals";

  const stacked = (label: string, content: string, testId: string) => (
    <Box key={testId}>
      <Box sx={{ color: ds.gray["500"], fontSize: 13 }}>{label}</Box>
      <Box
        data-testid={testId}
        sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600, mt: 0.75, wordBreak: "break-word" }}
      >
        {content}
      </Box>
    </Box>
  );

  return (
    <>
      <Box>
        <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 24, md: 28 }, fontWeight: 600, letterSpacing: "-0.5px", m: 0 }}>
          {t("createGA.review.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 15, lineHeight: "22px", m: 0, mt: 1.25 }}>
          {t("createGA.review.body")}
        </Box>
      </Box>
      <Box
        component="section"
        data-testid="create-ga-review"
        sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, boxShadow: SHADOWS.card, px: { xxs: 2.5, md: 4 }, py: { xxs: 2.5, md: 3.5 } }}
      >
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
          {stacked(
            t("createGA.review.type"),
            type ? t(`createGA.type.names.${type}`) : "",
            contentTestId("governance_action_type"),
          )}
          {stacked(labelOf("title"), String(value("title") ?? ""), contentTestId("title"))}
        </Box>
        <Box aria-hidden sx={divider} />
        <Box component="h2" sx={sectionTitle}>{t("createGA.details.about")}</Box>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, mt: 2.5 }}>
          {ABOUT_FIELDS.filter((key) => value(key)).map((key) => (
            <Box key={key}>
              <Box sx={{ color: ds.gray["600"], fontSize: 14, fontWeight: 600 }}>{labelOf(key)}</Box>
              <Box
                data-testid={contentTestId(key)}
                sx={{ color: ds.gray["900"], fontSize: 14, lineHeight: "20px", mt: 1, whiteSpace: "pre-wrap", wordBreak: "break-word" }}
              >
                {String(value(key))}
              </Box>
            </Box>
          ))}
        </Box>
        {detailKeys.length > 0 && (
          <>
            <Box aria-hidden sx={divider} />
            <Box component="h2" sx={sectionTitle}>
              {isTreasury ? t("createGA.details.address") : t("createGA.details.parameters")}
            </Box>
            <Box component="dl" sx={{ display: "grid", gap: 2, gridTemplateColumns: { xxs: "1fr", sm: "200px 1fr" }, m: 0, mt: 2.5 }}>
              {detailKeys.map((key) => (
                <Fragment key={key}>
                  <Box component="dt" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{labelOf(key)}</Box>
                  <Box
                    component="dd"
                    data-testid={contentTestId(key)}
                    sx={{ color: ds.gray["800"], fontFamily: FONT_FAMILY.mono, fontSize: 13, m: 0, mb: { xxs: 1, sm: 0 }, whiteSpace: "pre-wrap", wordBreak: "break-all" }}
                  >
                    {String(value(key))}
                  </Box>
                </Fragment>
              ))}
            </Box>
          </>
        )}
        {values.surveyTxId?.trim() && (
          <>
            <Box aria-hidden sx={divider} />
            <Box component="h2" sx={sectionTitle}>{t("createGA.details.survey")}</Box>
            <Box
              data-testid={contentTestId("surveyTxId")}
              sx={{ color: ds.gray["800"], fontFamily: FONT_FAMILY.mono, fontSize: 13, mt: 2, wordBreak: "break-all" }}
            >
              {values.surveyTxId.trim()}
            </Box>
          </>
        )}
        {links.length > 0 && (
          <>
            <Box aria-hidden sx={divider} />
            <Box component="h2" sx={sectionTitle}>{t("createGA.details.links")}</Box>
            <Box component="ul" sx={{ display: "flex", flexDirection: "column", gap: 1.5, listStyle: "none", m: 0, mt: 2.5, p: 0 }}>
              {links.map(({ uri, id }, index) => (
                <Box key={id} component="li" sx={{ display: "flex", flexDirection: { xxs: "column", sm: "row" }, gap: { xxs: 0.25, sm: 0 } }}>
                  <Box sx={{ color: ds.gray["900"], flexShrink: 0, fontSize: 14, fontWeight: 600, width: { sm: 200 } }}>
                    {`${t("forms.link")} ${index + 1}`}
                  </Box>
                  {/* openInNewTab makes the link absolute, so a typed scheme never runs. */}
                  <Box
                    component="button"
                    data-testid={`governance-action-link-${index + 1}-content`}
                    onClick={() => openInNewTab(uri)}
                    sx={{ background: "none", border: "none", color: ds.primary["600"], cursor: "pointer", fontFamily: "inherit", fontSize: 14, p: 0, textAlign: "left", wordBreak: "break-all", "&:hover": { textDecoration: "underline" }, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 } }}
                    type="button"
                  >
                    {uri}
                  </Box>
                </Box>
              ))}
            </Box>
          </>
        )}
      </Box>
      <CenteredBoxBottomButtons
        actionButtonText={t("createGA.review.continue")}
        backButtonText={t("createGA.review.edit")}
        onActionButton={() => setStep(5)}
        onBackButton={() => setStep(3)}
      />
    </>
  );
};
