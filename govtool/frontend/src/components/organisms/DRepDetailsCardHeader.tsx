import { useNavigate } from "react-router";
import { Trans } from "react-i18next";
import { Box, Chip } from "@mui/material";

import { Button } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { DataMissingHeader } from "@molecules";
import { correctDRepDirectoryFormat } from "@utils";
import { DRepData, MetadataValidationStatus } from "@/models";

type DRepDetailsProps = {
  dRepData: DRepData;
  isMe?: boolean;
  isMyDrep?: boolean;
  isValidating?: boolean;
  metadataStatus?: MetadataValidationStatus;
};

export const DRepDetailsCardHeader = ({
  dRepData,
  isMe,
  isMyDrep,
  isValidating,
  metadataStatus,
}: DRepDetailsProps) => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { screenWidth } = useScreenDimension();
  const { stakeKey } = useCardano();
  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);

  const { givenName, image } = dRepData;
  // A DRep's own page leaves out what it delegated to itself (#3082).
  const showMyDelegation = isMyDrep && !isMe && votingPower !== undefined;

  const navigateToEditDRep = () => {
    navigate(PATHS.editDrepMetadata, {
      state: dRepData,
    });
  };

  return (
    <div>
      {(isMe || isMyDrep) && (
        <Box
          sx={{
            alignSelf: "stretch",
            display: "flex",
            alignItems: "center",
            justifyContent: "flex-end",
            mb: "18px",
            ...(screenWidth <= 1020 && {
              flexDirection: "column",
              gap: 3,
            }),
          }}
        >
          {showMyDelegation && (
            <Chip
              color="primary"
              data-testid="my-delegation-chip"
              label={
                <Trans
                  i18nKey="dRepDirectory.myDRep"
                  values={{ ada: correctDRepDirectoryFormat(votingPower) }}
                />
              }
              sx={{
                boxShadow: (theme) => theme.shadows[2],
                color: (theme) => theme.palette.text.primary,
                px: 3,
                py: 0.5,
                width: "100%",
              }}
            />
          )}
          {isMe && (
            <Box
              sx={{
                alignItems: "center",
                display: "flex",
                gap: 1,
                ...(screenWidth < 1020 && {
                  width: "100%",
                }),
              }}
            >
              <Button
                data-testid="edit-drep-data-button"
                onClick={navigateToEditDRep}
                variant="outlined"
                sx={{
                  ...(screenWidth < 1020 && {
                    width: "100%",
                  }),
                }}
              >
                {t("dRepDirectory.editBtn")}
                <img
                  alt="sorting active"
                  src={ICONS.editIcon}
                  style={{ marginLeft: "4px" }}
                />
              </Button>
            </Box>
          )}
        </Box>
      )}
      <DataMissingHeader
        isDRep
        title={givenName ?? undefined}
        image={image}
        isDataMissing={metadataStatus}
        titleStyle={{ wordBreak: "break-word", whiteSpace: "wrap" }}
        isValidating={isValidating}
      />
    </div>
  );
};
