import { Box, Skeleton } from "@mui/material";
import { useQuery } from "@tanstack/react-query";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";
import {
  IconCheckCircle,
  IconXCircle,
} from "@intersect.mbo/intersectmbo.org-icons-set";

import { Tooltip, Typography } from "@atoms";
import { QUERY_KEYS, successGreen } from "@consts";
import { useTranslation } from "@hooks";
import { postOutcomeVerifySignature } from "@services";

// A CIP-108 author field is a plain string or a JSON-LD `{ "@value": … }`.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const getValue = (field: any): string => {
  if (typeof field === "string") return field;
  if (field && typeof field === "object" && field["@value"]) {
    return field["@value"];
  }
  return "";
};

type OutcomeAuthorProps = {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  author: any;
  metadataUrl?: string;
};

const OutcomeAuthor = ({ author, metadataUrl }: OutcomeAuthorProps) => {
  const { t } = useTranslation();

  const name = getValue(author?.name);
  const witnessAlgorithm = getValue(author?.witness?.witnessAlgorithm);
  const publicKey = getValue(author?.witness?.publicKey);
  const signature = getValue(author?.witness?.signature);
  const canVerify =
    !!metadataUrl && !!name && !!witnessAlgorithm && !!publicKey && !!signature;

  const { data: verification, isLoading } = useQuery({
    queryKey: [
      QUERY_KEYS.useOutcomeVerifySignatureKey,
      metadataUrl,
      name,
      witnessAlgorithm,
      publicKey,
      signature,
    ],
    queryFn: () =>
      postOutcomeVerifySignature({
        author: { name, witness: { witnessAlgorithm, publicKey, signature } },
        metadataUrl: metadataUrl as string,
      }).catch(() => ({
        isValid: false,
        author: name,
        error: "Failed to verify signature",
      })),
    enabled: canVerify,
    retry: false,
  });

  const renderVerificationIcon = () => {
    if (canVerify && isLoading) {
      return <Skeleton variant="circular" width={20} height={20} />;
    }
    if (!verification) return null;
    if (verification.isValid) {
      return (
        <Tooltip heading={t("outcome.authors.witnessVerified")}>
          <Box display="flex">
            <IconCheckCircle fill={successGreen.c400} width={20} height={20} />
          </Box>
        </Tooltip>
      );
    }
    return (
      <Tooltip
        paragraphOne={t("outcome.authors.verificationFailed")}
        paragraphTwo={
          ("message" in verification && verification.message) ||
          verification.error ||
          "Invalid signature"
        }
      >
        <Box display="flex">
          <IconXCircle fill="#ef4444" width={19} height={19} />
        </Box>
      </Tooltip>
    );
  };

  return (
    <Box display="flex" alignItems="center" gap={0.5}>
      {renderVerificationIcon()}
      <Typography sx={{ fontSize: 16, fontWeight: 400 }}>{name}</Typography>
      <Tooltip
        heading={`${t("outcome.authors.witnessAlgorithm")}: ${witnessAlgorithm}`}
        paragraphOne={`${t("outcome.authors.publicKey")}: ${publicKey}`}
        paragraphTwo={`${t("outcome.authors.signature")}: ${signature}`}
      >
        <InfoOutlinedIcon sx={{ fontSize: "19px", color: "#ADAEAD" }} />
      </Tooltip>
    </Box>
  );
};

type OutcomeAuthorsProps = {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  authors?: any[];
  metadataUrl?: string;
};

/** Authors of the action's metadata, each with its witness checked by the outcomes API. */
export const OutcomeAuthors = ({
  authors,
  metadataUrl,
}: OutcomeAuthorsProps) => {
  const { t } = useTranslation();

  return (
    <Box
      data-testid="single-action-authors"
      display="flex"
      flexDirection="column"
      gap={0.5}
    >
      <Typography sx={{ color: "neutralGray", fontWeight: 600, fontSize: 14 }}>
        {t("outcome.authors.title")}
      </Typography>
      {authors?.length ? (
        <Box display="flex" gap={2} flexWrap="wrap">
          {authors.map((author, index) => (
            <OutcomeAuthor
              // Authors have no id; the list is fixed for a given action.
              // eslint-disable-next-line react/no-array-index-key
              key={index}
              author={author}
              metadataUrl={metadataUrl}
            />
          ))}
        </Box>
      ) : (
        <Typography
          sx={{ fontSize: 16, fontWeight: 400, color: "neutralGray" }}
        >
          {t("outcome.authors.noDataAvailable")}
        </Typography>
      )}
    </Box>
  );
};
