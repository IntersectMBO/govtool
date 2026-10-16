import { Box } from "@mui/material";
import { Diff, Hunk, parseDiff } from "react-diff-view";
import { diffLines, formatLines } from "unidiff";
import "react-diff-view/style/index.css";

import { CopyButton, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { GovernanceActionRecord } from "@models";
import { encodeGovernanceActionCommitteeColdId } from "@utils";

type CCMember = {
  expirationEpoch: number | null;
  hasScript: boolean;
  hash: string;
  newExpirationEpoch?: number | null;
};

type CCMemberToBeRemoved = {
  hash: string;
  hasScript: boolean;
};

const sectionTitleSx = {
  fontSize: 14,
  fontWeight: 600,
  lineHeight: "20px",
  color: "neutralGray",
  overflow: "hidden",
  wordBreak: "break-word",
} as const;

const memberIdSx = {
  fontSize: 16,
  fontWeight: 400,
  lineHeight: "24px",
  color: "primaryBlue",
  wordBreak: "break-word",
} as const;

const EpochDiffView = ({
  expirationEpoch,
  newExpirationEpoch,
}: {
  expirationEpoch?: number | null;
  newExpirationEpoch?: number | null;
}) => {
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();
  const isSmallScreen = screenWidth < 600;

  const diffText = formatLines(
    diffLines(
      JSON.stringify(
        { [t("actionRecord.expirationEpoch")]: expirationEpoch ?? "-" },
        null,
        2,
      ),
      JSON.stringify(
        { [t("actionRecord.newExpirationEpoch")]: newExpirationEpoch ?? "-" },
        null,
        2,
      ),
    ),
  );
  const [diff] = parseDiff(diffText, {});

  return (
    <Box sx={{ mt: 1 }}>
      <Diff
        viewType={isSmallScreen ? "unified" : "split"}
        diffType={diff.type}
        hunks={diff.hunks || []}
      >
        {(hunks) =>
          hunks.map((hunk) => (
            // Hunk's typings omit children, which its docs pass.
            // eslint-disable-next-line @typescript-eslint/ban-ts-comment
            // @ts-expect-error
            <Hunk key={hunk.content} hunk={hunk}>
              {hunk.content}
            </Hunk>
          ))
        }
      </Diff>
    </Box>
  );
};

const MemberId = ({ id, dataTestId }: { id: string; dataTestId?: string }) => (
  <Box display="flex" flexDirection="row" alignItems="center" gap={1}>
    <Typography data-testid={dataTestId} sx={memberIdSx}>
      {id}
    </Typography>
    <CopyButton text={id} variant="blueThin" />
  </Box>
);

/**
 * The committee changes of an UpdateCommittee action: added, removed and
 * re-termed members, and the new threshold.
 */
export const GovernanceActionNewCommitteeDetails = ({
  description,
}: Pick<GovernanceActionRecord, "description">) => {
  const { t } = useTranslation();

  const members = (description?.members as CCMember[] | undefined) ?? [];
  const toBeRemoved =
    (description?.membersToBeRemoved as CCMemberToBeRemoved[] | undefined) ??
    [];

  const withId = <T extends { hash: string; hasScript: boolean }>(
    member: T,
  ) => ({
    ...member,
    cip129Identifier: encodeGovernanceActionCommitteeColdId(
      member.hash,
      member.hasScript,
    ),
  });

  const membersToBeAdded = members
    .filter(
      (member) =>
        (member?.expirationEpoch === undefined ||
          member?.expirationEpoch === null) &&
        member?.hash,
    )
    .map(withId);
  const membersToBeUpdated = members
    .filter(
      (member) =>
        !!member?.expirationEpoch &&
        !!member?.newExpirationEpoch &&
        member?.hash,
    )
    .map(withId);
  const membersRemoved = toBeRemoved
    .filter((member) => member?.hash && member.hash.trim() !== "")
    .map(withId);

  return (
    <Box display="flex" flexDirection="column" gap={3}>
      {membersToBeAdded.length > 0 && (
        <Box
          data-testid="members-to-be-added-to-the-committee"
          display="flex"
          flexDirection="column"
          gap={0.5}
        >
          <Typography sx={sectionTitleSx}>
            {t("actionRecord.membersToBeAddedToCommittee")}
          </Typography>
          {membersToBeAdded.map(
            ({ cip129Identifier, hash, newExpirationEpoch }) => (
              <Box key={hash} display="flex" flexDirection="column">
                <MemberId
                  id={cip129Identifier}
                  dataTestId={`member-to-be-added-to-the-committee-id-${cip129Identifier}`}
                />
                <Typography
                  data-testid="member-expiration-date"
                  sx={{
                    fontSize: 14,
                    lineHeight: "24px",
                    color: "neutralGray",
                  }}
                >
                  {`${t("actionRecord.expirationEpoch")} ${
                    newExpirationEpoch ?? "-"
                  }`}
                </Typography>
              </Box>
            ),
          )}
        </Box>
      )}

      {membersRemoved.length > 0 && (
        <Box
          data-testid="members-to-be-removed-from-the-committee"
          display="flex"
          flexDirection="column"
          gap={0.5}
        >
          <Typography sx={sectionTitleSx}>
            {t("actionRecord.membersToBeRemovedToCommittee")}
          </Typography>
          {membersRemoved.map(({ hash, cip129Identifier }) => (
            <MemberId
              key={hash}
              id={cip129Identifier}
              dataTestId="members-to-be-removed-from-the-committee-id"
            />
          ))}
        </Box>
      )}

      {membersToBeUpdated.length > 0 && (
        <Box
          data-testid="change-to-terms-of-existing-members"
          display="flex"
          flexDirection="column"
          gap={0.5}
        >
          <Typography sx={sectionTitleSx}>
            {t("actionRecord.changeToTermsOfExistingMembers")}
          </Typography>
          {membersToBeUpdated.map(
            ({
              cip129Identifier,
              newExpirationEpoch,
              expirationEpoch,
              hash,
            }) => (
              <Box
                key={hash}
                display="flex"
                flexDirection="column"
                data-testid={`${cip129Identifier}-member-id`}
              >
                <MemberId id={cip129Identifier} />
                <EpochDiffView
                  expirationEpoch={expirationEpoch}
                  newExpirationEpoch={newExpirationEpoch}
                />
              </Box>
            ),
          )}
        </Box>
      )}

      {description?.threshold != null && (
        <Box
          data-testid="new-threshold-container"
          display="flex"
          flexDirection="column"
          gap={0.5}
        >
          <Typography sx={sectionTitleSx}>
            {t("actionRecord.newThreshold")}
          </Typography>
          <Typography
            data-testid="new-threshold-value"
            sx={{ fontSize: 16, lineHeight: "24px", wordBreak: "break-word" }}
          >
            {String(description.threshold)}
          </Typography>
        </Box>
      )}
    </Box>
  );
};
