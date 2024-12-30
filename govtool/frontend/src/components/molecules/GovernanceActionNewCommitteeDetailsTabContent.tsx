import { useTranslation } from "react-i18next";
import { Box } from "@mui/material";

import { ProposalData } from "@/models";
import { encodeCIP129Identifier } from "@/utils";

import { CopyButton, Typography } from "../atoms";

type CCMember = {
  expirationEpoch: number;
  hasScript: boolean;
  hash: string;
  newExpirationEpoch?: number;
};

export const GovernanceActionNewCommitteeDetailsTabContent = ({
  details,
}: Pick<ProposalData, "details">) => {
  const { t } = useTranslation();
  const membersToBeAdded = ((details?.members as CCMember[]) || [])
    .filter((member) => member.newExpirationEpoch === undefined)
    .map((member) => ({
      cip129Identifier: encodeCIP129Identifier({
        txID: (member.hasScript ? "02" : "13") + member.hash,
        bech32Prefix: member.hasScript ? "cc_hot" : "cc_cold",
      }),
      expirationEpoch: member.expirationEpoch,
    }));

  const membersToBeUpdated = ((details?.members as CCMember[]) || [])
    .filter((member) => member.newExpirationEpoch !== undefined)
    .map((member) => ({
      cip129Identifier: encodeCIP129Identifier({
        txID: (member.hasScript ? "02" : "13") + member.hash,
        bech32Prefix: member.hasScript ? "cc_hot" : "cc_cold",
      }),
      expirationEpoch: member.expirationEpoch,
      newExpirationEpoch: member.newExpirationEpoch,
    }));

  return (
    <Box>
      {membersToBeAdded.length > 0 && (
        <Box mb="32px">
          <Typography
            sx={{
              fontSize: 14,
              fontWeight: 600,
              lineHeight: "20px",
              color: "neutralGray",
              overflow: "hidden",
              textOverflow: "ellipsis",
              whiteSpace: "nowrap",
            }}
          >
            {t("govActions.membersToBeAdded")}
          </Typography>
          {membersToBeAdded.map(({ cip129Identifier }) => (
            <Box display="flex" flexDirection="row">
              <Typography
                sx={{
                  fontSize: 16,
                  fontWeight: 400,
                  maxWidth: "auto",
                  lineHeight: "24px",
                  color: "primaryBlue",
                }}
              >
                {cip129Identifier}
              </Typography>
              <Box ml={1}>
                <CopyButton
                  text={cip129Identifier.toString()}
                  variant="blueThin"
                />
              </Box>
            </Box>
          ))}
        </Box>
      )}
      {(details?.membersToBeRemoved as string[]).length > 0 && (
        <Box mb="32px">
          <Typography
            sx={{
              fontSize: 14,
              fontWeight: 600,
              lineHeight: "20px",
              color: "neutralGray",
              overflow: "hidden",
              textOverflow: "ellipsis",
              whiteSpace: "nowrap",
            }}
          >
            {t("govActions.membersToBeRemoved")}
          </Typography>
          {(details?.membersToBeRemoved as string[]).map((hash) => (
            <Box display="flex" flexDirection="row">
              <Typography
                sx={{
                  fontSize: 16,
                  fontWeight: 400,
                  maxWidth: "auto",
                  lineHeight: "24px",
                  color: "primaryBlue",
                }}
              >
                {encodeCIP129Identifier({
                  txID: hash,
                  bech32Prefix: "cc_cold",
                })}
              </Typography>
              <Box ml={1}>
                <CopyButton
                  text={encodeCIP129Identifier({
                    txID: hash,
                    bech32Prefix: "cc_cold",
                  })}
                  variant="blueThin"
                />
              </Box>
            </Box>
          ))}
        </Box>
      )}

      {membersToBeUpdated.length > 0 && (
        <Box mb="32px">
          <Typography
            sx={{
              fontSize: 14,
              fontWeight: 600,
              lineHeight: "20px",
              color: "neutralGray",
              overflow: "hidden",
              textOverflow: "ellipsis",
              whiteSpace: "nowrap",
            }}
          >
            {t("govActions.changeToTermsOfExistingMembers")}
          </Typography>
          {membersToBeUpdated.map(
            ({ cip129Identifier, newExpirationEpoch, expirationEpoch }) => (
              <>
                <Box display="flex" flexDirection="row">
                  <Typography
                    sx={{
                      fontSize: 16,
                      fontWeight: 400,
                      maxWidth: "auto",
                      lineHeight: "24px",
                      color: "primaryBlue",
                    }}
                  >
                    {cip129Identifier}
                  </Typography>
                  <Box ml={1}>
                    <CopyButton
                      text={cip129Identifier.toString()}
                      variant="blueThin"
                    />
                  </Box>
                </Box>
                <Typography
                  sx={{
                    fontSize: 14,
                    fontWeight: 400,
                    lineHeight: "24px",
                    color: "neutralGray",
                  }}
                >
                  {t("govActions.changeToTermsEpochs", {
                    epochTo: newExpirationEpoch,
                    epochFrom: expirationEpoch,
                  })}
                </Typography>
              </>
            ),
          )}
        </Box>
      )}
      {details?.threshold && (
        <Box>
          <Typography
            sx={{
              fontSize: 14,
              fontWeight: 600,
              lineHeight: "20px",
              color: "neutralGray",
              overflow: "hidden",
              textOverflow: "ellipsis",
              whiteSpace: "nowrap",
            }}
          >
            {t("govActions.newThresholdValue")}
          </Typography>
          <Typography
            sx={{
              fontSize: 16,
              fontWeight: 400,
              maxWidth: "auto",
              lineHeight: "24px",
            }}
          >
            {(details?.threshold as number).toString()}
          </Typography>
        </Box>
      )}
    </Box>
  );
};
