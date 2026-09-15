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
type CCMemberToBeRemoved = {
  hash: string;
  hasScript?: boolean;
};

const getCip129Identifier = (hash: string, hasScript?: boolean) =>
  encodeCIP129Identifier({
    txID: (hasScript ? "13" : "12") + hash,
    bech32Prefix: "cc_cold",
  });

export const GovernanceActionNewCommitteeDetailsTabContent = ({
  details,
}: Pick<ProposalData, "details">) => {
  const { t } = useTranslation();
  const membersToBeAdded = ((details?.members as CCMember[]) || [])
    .filter(
      (member) =>
        member?.expirationEpoch === undefined ||
        member?.expirationEpoch === null,
    )
    .filter((member) => member?.hash)
    .map((member) => ({
      cip129Identifier: getCip129Identifier(member.hash, member.hasScript),
      expirationEpoch: member.expirationEpoch,
    }));

  const membersToBeUpdated = ((details?.members as CCMember[]) || [])
    .filter(
      (member) => !!member?.expirationEpoch && !!member?.newExpirationEpoch,
    )
    .filter((member) => member?.hash)
    .map((member) => ({
      cip129Identifier: getCip129Identifier(member.hash, member.hasScript),
      expirationEpoch: member.expirationEpoch,
      newExpirationEpoch: member.newExpirationEpoch,
    }));

  const membersToBeRemoved = (
    (details?.membersToBeRemoved as CCMemberToBeRemoved[]) || []
  )
    .filter((member) => member?.hash && member.hash.trim() !== "")
    .map((member) => ({
      cip129Identifier: getCip129Identifier(member.hash, member.hasScript),
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
            {t("govActions.membersToBeAddedToTheCommittee")}
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
      {membersToBeRemoved.length > 0 && (
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
            {t("govActions.membersToBeRemovedFromTheCommittee")}
          </Typography>
          {membersToBeRemoved.map(({ cip129Identifier }) => (
            <Box display="flex" flexDirection="row" key={cip129Identifier}>
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
                <CopyButton text={cip129Identifier} variant="blueThin" />
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
                    epochTo: newExpirationEpoch ?? "N/A",
                    epochFrom: expirationEpoch ?? "N/A",
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
