import { Box, Skeleton } from "@mui/material";
import { FC, ReactNode } from "react";
import {
  ArrowRight,
  BadgeCheck,
  CirclePlus,
  ListChecks,
  LucideIcon,
  Users,
  Vote,
} from "lucide-react";

import { Button, LoadingButtonProps, Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { useAppContext } from "@context";
import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";

export type DashboardActionCardProps = {
  buttons?: LoadingButtonProps[];
  children?: ReactNode;
  description?: ReactNode;
  imageURL?: string;
  isLoading?: boolean;
  isInProgressOnCard?: boolean;
  state?: "active" | "inProgress" | "default";
  title?: ReactNode;
  transactionId?: string | null;
  type:
    | "delegate"
    | "d-rep"
    | "direct-voter"
    | "list-gov-actions"
    | "propose-gov-action";
  isSpaceBetweenButtons?: boolean;
};

const TYPE_ICONS: Record<DashboardActionCardProps["type"], { Icon: LucideIcon; tone: string; bg: string }> = {
  delegate: { Icon: Users, tone: ds.primary["600"], bg: ds.primary["50"] },
  "d-rep": { Icon: BadgeCheck, tone: ds.orange["600"], bg: ds.orange["50"] },
  "direct-voter": { Icon: Vote, tone: ds.orange["600"], bg: ds.orange["50"] },
  "list-gov-actions": { Icon: ListChecks, tone: ds.primary["600"], bg: ds.primary["50"] },
  "propose-gov-action": { Icon: CirclePlus, tone: ds.success["600"], bg: ds.success["50"] },
};

/** A dashboard row: icon, title, description, then its buttons on the right. */
export const DashboardActionCard: FC<DashboardActionCardProps> = ({
  buttons,
  children,
  description,
  isLoading = false,
  state = "default",
  title,
  type,
  transactionId,
}) => {
  const { t } = useTranslation();
  const { cExplorerBaseUrl } = useAppContext();
  const { Icon, tone, bg } = TYPE_ICONS[type];
  const isInProgress = state === "inProgress";

  return (
    <Box
      data-testid={`${type}-dashboard-card`}
      sx={{
        alignItems: { md: "center" },
        backgroundColor: "#FFFFFF",
        border: `1px solid ${isInProgress ? "#FFCBAD" : state === "active" ? ds.primary["200"] : ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: { xxs: "column", md: "row" },
        gap: { xxs: 2, md: 3 },
        p: { xxs: 2, md: 2.75 },
        position: "relative",
      }}
    >
      <Box sx={{ display: "flex", flex: 1, gap: 2, minWidth: 0 }}>
        <Box
          sx={{
            alignItems: "center",
            backgroundColor: bg,
            borderRadius: `${RADIUS.md}px`,
            color: tone,
            display: "flex",
            flexShrink: 0,
            height: 40,
            justifyContent: "center",
            width: 40,
          }}
        >
          <Icon aria-hidden size={20} />
        </Box>
        <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 0.75, minWidth: 0 }}>
          {title && (
            <Box component="h2" sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", flexWrap: "wrap", fontSize: 17, fontWeight: 600, gap: 1, lineHeight: "24px", m: 0 }}>
              {isLoading ? <Skeleton variant="text" width={180} /> : <span>{title}</span>}
              {isInProgress && !isLoading && (
                <Box
                  component="span"
                  data-testid={`${type}-in-progress`}
                  sx={{ backgroundColor: PILL_TONES.warning.bg, border: `1px solid ${PILL_TONES.warning.border}`, borderRadius: `${RADIUS.xs}px`, color: PILL_TONES.warning.text, fontSize: 12, fontWeight: 500, lineHeight: "18px", px: 0.75 }}
                >
                  {t("inProgress")}
                </Box>
              )}
            </Box>
          )}
          {description && (
            <Typography
              color={ds.gray["600"]}
              component="div"
              data-testid="voting-power-delegation-status"
              fontWeight={400}
              sx={{ fontSize: 14, lineHeight: "20px" }}
            >
              {isLoading ? <Skeleton variant="rounded" height={40} /> : description}
            </Typography>
          )}
          {children}
          {transactionId && (
            <Box
              component="a"
              data-testid={`${type}-show-transaction`}
              href={`${cExplorerBaseUrl}/tx/${transactionId}`}
              rel="noopener noreferrer"
              sx={{ alignItems: "center", alignSelf: "flex-start", color: ds.primary["600"], display: "inline-flex", fontSize: 14, fontWeight: 600, gap: 0.5, textDecoration: "underline" }}
              target="_blank"
            >
              {t("dashboard.cards.showTransaction")}
              <ArrowRight aria-hidden size={14} />
            </Box>
          )}
        </Box>
      </Box>
      {!!buttons?.length && (
        <Box
          sx={{
            display: "flex",
            flexDirection: { xxs: "column", sm: "row" },
            flexShrink: 0,
            gap: 1.25,
          }}
        >
          {isLoading ? (
            <Skeleton animation="wave" height={36} variant="rounded" width={120} />
          ) : (
            buttons.map(({ dataTestId, ...buttonProps }, position) => (
              <Button
                key={buttonProps.children?.toString()}
                data-testid={dataTestId}
                size="medium"
                variant={position === 0 ? "soft" : "outlined"}
                {...buttonProps}
                sx={{
                  whiteSpace: "nowrap",
                  width: { xxs: "100%", sm: "auto" },
                  ...buttonProps.sx,
                }}
              />
            ))
          )}
        </Box>
      )}
    </Box>
  );
};
