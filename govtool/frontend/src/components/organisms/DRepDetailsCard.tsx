import { PropsWithChildren } from "react";
import { Box, ButtonBase, Link } from "@mui/material";

import { Button, ExternalModalButton, StatusPill, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useDelegateTodRep, useScreenDimension, useTranslation } from "@hooks";
import { Card, DataMissingInfoBox } from "@molecules";
import { correctAdaFormat, testIdFromLabel } from "@utils";
import { DRepData } from "@/models";
import { DRepDetailsCardHeader } from "./DRepDetailsCardHeader";

type DRepDetailsProps = {
  dRepData: DRepData;
  isConnected: boolean;
  isMe?: boolean;
  isMyDrep?: boolean;
  isMyDrepInProgress?: boolean;
};

export const DRepDetailsCard = ({
  dRepData,
  isConnected,
  isMe,
  isMyDrep,
  isMyDrepInProgress,
}: DRepDetailsProps) => {
  const { pendingTransaction } = useCardano();
  const { t } = useTranslation();
  const { openModal } = useModal();
  const { screenWidth } = useScreenDimension();
  const { delegate, isDelegating } = useDelegateTodRep();

  const {
    metadataStatus,
    motivations,
    objectives,
    paymentAddress,
    qualifications,
    references,
    status,
    url,
    view,
    votingPower,
  } = dRepData;

  const groupedReferences = references?.reduce<Record<string, Reference[]>>(
    (acc, reference) => {
      const type = reference["@type"];
      if (!acc[type]) {
        acc[type] = [];
      }
      acc[type].push(reference);
      return acc;
    },
    {},
  );

  const linkReferences = groupedReferences?.Link;
  const identityReferences = groupedReferences?.Identity;

  return (
    <Card
      {...((isMe || isMyDrep) && {
        variant: "primary",
      })}
      {...(isMyDrepInProgress && {
        variant: "warning",
        label: t("inProgress"),
      })}
      sx={{
        borderRadius: 5,
        pb: 4.25,
        pt: 2.25,
        display: "flex",
        flexDirection: "column",
        gap: 6,
      }}
    >
      {/* BASIC INFO */}
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          gap: 2,
        }}
      >
        <DRepDetailsCardHeader
          dRepData={dRepData}
          isMe={isMe}
          isMyDrep={isMyDrep}
        />
        {/* ERROR MESSAGES */}
        {metadataStatus && (
          <DataMissingInfoBox
            isDataMissing={metadataStatus}
            isDrep
            sx={{ mb: 0 }}
          />
        )}
        {metadataStatus && !!url && (
          <ExternalModalButton
            label={t("govActions.seeExternalData")}
            sx={{ mb: 0, alignSelf: "flex-start" }}
            url={url}
          />
        )}
        {/* ERROR MESSAGES END */}
        <DRepDetailsInfoItem label={t("drepId")} dataTestId="drep-id">
          <CopyableText value={view} dataTestId="copy-drep-id-button" />
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem label={t("status")} dataTestId="drep-status">
          <StatusPill status={status} />
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem
          label={t("votingPower")}
          dataTestId="drep-voting-power"
        >
          <Typography
            data-testid="voting-power"
            sx={{ display: "flex", flexDirection: "row", mt: 0.5 }}
          >
            {"₳ "}
            {correctAdaFormat(votingPower)}
          </Typography>
        </DRepDetailsInfoItem>
      </Box>
      {/* BASIC INFO END */}

      {/* BUTTONS */}
      {isConnected && status === "Active" && !isMyDrep && (
        <Button
          data-testid="delegate-button"
          disabled={!!pendingTransaction?.delegate}
          isLoading={
            isDelegating === dRepData.view || isDelegating === dRepData.drepId
          }
          onClick={() => delegate(dRepData.drepId)}
          size="extraLarge"
          sx={{ width: "100%", maxWidth: screenWidth < 1024 ? "100%" : 286 }}
          variant="contained"
        >
          {t("delegate")}
        </Button>
      )}
      {!isConnected && status === "Active" && (
        <Button
          data-testid="connect-to-delegate-button"
          onClick={() =>
            openModal({
              type: "chooseWallet",
              state: {
                pathToNavigate: PATHS.dashboardDRepDirectoryDRep.replace(
                  ":dRepId",
                  view,
                ),
              },
            })
          }
          size="extraLarge"
          sx={{ width: "100%", maxWidth: screenWidth < 1024 ? "100%" : 286 }}
          variant="outlined"
        >
          {t("connectToDelegate")}
        </Button>
      )}
      {/* BUTTONS END */}

      {/* CIP-119 DATA */}
      {!metadataStatus && (
        <>
          <DRepDetailsInfoItem
            label={t("forms.dRepData.objectives")}
            text={objectives}
            dataTestId="objectives"
          />
          <DRepDetailsInfoItem
            label={t("forms.dRepData.motivations")}
            text={motivations}
            dataTestId="motivations"
          />
          <DRepDetailsInfoItem
            label={t("forms.dRepData.qualifications")}
            text={qualifications}
            dataTestId="qualifications"
          />
          {linkReferences?.length > 0 && (
            <DRepDetailsInfoItem
              label={t("forms.dRepData.referenceTypes.link.title")}
              dataTestId="references-link"
            >
              <ReferencesGroup references={linkReferences} />
            </DRepDetailsInfoItem>
          )}
          {identityReferences?.length > 0 && (
            <DRepDetailsInfoItem
              label={t("forms.dRepData.referenceTypes.identity.title")}
              dataTestId="references-identity"
            >
              <ReferencesGroup references={identityReferences} />
            </DRepDetailsInfoItem>
          )}
          <DRepDetailsInfoItem
            label={t("forms.dRepData.paymentAddress")}
            dataTestId="payment-address"
          >
            {paymentAddress && (
              <CopyableText
                value={paymentAddress}
                dataTestId="copy-payment-address-button"
              />
            )}
          </DRepDetailsInfoItem>
        </>
      )}
      {/* CIP-119 DATA END */}
    </Card>
  );
};

const ellipsisStyles = {
  overflow: "hidden",
  textOverflow: "ellipsis",
  whiteSpace: "nowrap",
};

type DrepDetailsInfoItemProps = PropsWithChildren & {
  label: string;
  text?: string | null;
  dataTestId: string;
};

const DRepDetailsInfoItem = ({
  children,
  label,
  text,
  dataTestId,
}: DrepDetailsInfoItemProps) => {
  if (!children && !text) return null;
  const dataTestIdInfoItemCategoryPrefix = "info-item";
  return (
    <Box
      sx={{
        maxWidth: {
          xxs: "295px",
          md: "100%",
        },
      }}
    >
      <Box
        sx={{
          mb: 0.5,
        }}
      >
        <Typography
          color="neutralGray"
          fontWeight={600}
          variant="body2"
          data-testid={`${dataTestId}-${dataTestIdInfoItemCategoryPrefix}-title`}
        >
          {label}
        </Typography>
      </Box>
      <div
        data-testid={`${dataTestId}-${dataTestIdInfoItemCategoryPrefix}-description`}
      >
        {text && (
          <Typography fontWeight={400} sx={{ maxWidth: 608 }} variant="body1">
            {text}
          </Typography>
        )}
        {children}
      </div>
    </Box>
  );
};

type CopyableTextProps = {
  value: string;
  dataTestId: string;
};

const CopyableText = ({ value, dataTestId }: CopyableTextProps) => (
  <ButtonBase
    onClick={(e) => {
      navigator.clipboard.writeText(value.toString());
      e.stopPropagation();
    }}
    data-testid={dataTestId}
    sx={{
      gap: 1,
      maxWidth: "100%",
      "&:hover": {
        opacity: 0.6,
        transition: "opacity 0.3s",
      },
    }}
  >
    <Typography color="primary" fontWeight={500} sx={ellipsisStyles}>
      {value}
    </Typography>
    <img alt="" src={ICONS.copyBlueIcon} />
  </ButtonBase>
);

type ReferenceItem = {
  label: string;
  uri: string;
};

const ReferencesGroup = ({ references }: { references: ReferenceItem[] }) => (
  <Box display="flex" flexDirection="column" gap={3}>
    {references.map(({ label, uri }) => (
      <ReferencesLink key={uri} label={label} uri={uri} />
    ))}
  </Box>
);

const ReferencesLink = ({ label, uri }: ReferenceItem) => (
  <Box
    sx={{
      display: "flex",
      flexDirection: "column",
      gap: 0.5,
      maxWidth: {
        xxs: "295px",
        md: "100%",
      },
    }}
  >
    <Typography fontWeight={400}>{label}</Typography>
    <Link
      data-testid={`${testIdFromLabel(label)}-link`}
      href={uri}
      target="_blank"
      sx={{
        overflow: "hidden",
        textOverflow: "ellipsis",
        display: "flex",
        gap: 1,
        alignItems: "center",
      }}
    >
      <Typography
        color="primary"
        fontWeight={400}
        sx={{
          overflow: "hidden",
          textOverflow: "ellipsis",
        }}
      >
        {uri}
      </Typography>
      <img alt="link" height={16} src={ICONS.externalLinkIcon} width={16} />
    </Link>
  </Box>
);
