import { useNavigate } from "react-router-dom";
import { Box, Chip } from "@mui/material";

import { Button } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { DataMissingHeader, Share } from "@molecules";
import { correctDRepDirectoryFormat } from "@utils";
import { DRepData } from "@/models";

type DRepDetailsProps = {
  dRepData: DRepData;
  isMe?: boolean;
  isMyDrep?: boolean;
};

export const DRepDetailsCardHeader = ({
  dRepData,
  isMe,
  isMyDrep,
}: DRepDetailsProps) => {
  const { stakeKey } = useCardano();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { screenWidth } = useScreenDimension();
  const { votingPower: myVotingPower } =
    useGetAdaHolderVotingPowerQuery(stakeKey);

  const { givenName, metadataStatus } = dRepData;

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
            justifyContent: "space-between",
            mb: "18px",
            ...(screenWidth <= 1020 && {
              flexDirection: "column",
              gap: 3,
            }),
          }}
        >
          <Chip
            color="primary"
            label={
              isMe
                ? isMyDrep
                  ? t("dRepDirectory.myDelegationToYourself")
                  : t("dRepDirectory.meAsDRep")
                : t("dRepDirectory.myDRep", {
                    ada: correctDRepDirectoryFormat(myVotingPower),
                  })
            }
            sx={{
              boxShadow: (theme) => theme.shadows[2],
              color: (theme) => theme.palette.text.primary,
              px: 3,
              py: 0.5,
              ...(isMyDrep &&
                !isMe && {
                  width: "100%",
                }),
              ...(screenWidth <= 1020 && {
                width: "100%",
              }),
            }}
          />

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
              {screenWidth >= 1020 && <Share link={window.location.href} />}
            </Box>
          )}
        </Box>
      )}
      <DataMissingHeader
        title={givenName ?? undefined}
        isDataMissing={metadataStatus}
        shareLink={
          !isMe || screenWidth < 1020 ? window.location.href : undefined
        }
        titleStyle={{ wordBreak: "break-word", whiteSpace: "wrap" }}
      />
    </div>
  );
};
