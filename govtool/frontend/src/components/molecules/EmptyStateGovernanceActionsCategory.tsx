import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { getProposalTypeLabel } from "@utils";

import { EmptyStateGovernanceActionsCategoryProps } from "./types";

export const EmptyStateGovernanceActionsCategory = ({
  category,
  isSearch,
}: EmptyStateGovernanceActionsCategoryProps) => {
  const { t } = useTranslation();

  return (
    <Typography
      sx={{
        fontWeight: 300,
        py: 4,
      }}
    >
      {isSearch ? (
        t("govActions.noResultsForTheSearch")
      ) : (
        <>
          {t("govActions.withCategoryNotExist.partOne")}
          &nbsp;
          <Typography
            sx={{
              display: "inline",
              fontWeight: 700,
            }}
          >
            {getProposalTypeLabel(category ?? "")}
          </Typography>
          &nbsp;
          {t("govActions.withCategoryNotExist.partTwo")}
        </>
      )}
    </Typography>
  );
};
