import { Box } from "@mui/material";
import DriveFileRenameOutlineOutlinedIcon from "@mui/icons-material/DriveFileRenameOutlineOutlined";

import { Button, Spacer, Typography } from "@atoms";
import { ICONS } from "@consts";
import {
  defaulCreateGovernanceActionValues,
  useCreateGovernanceActionForm,
  useTranslation,
} from "@hooks";
import { LinkWithIcon } from "@molecules";
import { openInNewTab } from "@utils";

import { Dispatch, SetStateAction } from "react";
import { BgCard } from "../BgCard";

type ReviewCreatedGovernanceActionProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export function ReviewCreatedGovernanceAction({
  setStep,
}: ReviewCreatedGovernanceActionProps) {
  const { t } = useTranslation();
  const { getValues } = useCreateGovernanceActionForm();
  const values = getValues();

  const onClickContinue = () => {
    setStep(5);
  };

  const onClickBackButton = () => {
    setStep(3);
  };

  const onClickEditSubmission = () => {
    setStep(3);
  };

  const onClickLink = (link: string) => {
    openInNewTab(link);
  };

  const renderReviewFields = () => Object.entries(values)
    .filter(
      ([key]) => !Object.keys(defaulCreateGovernanceActionValues).includes(key)
          || key === "governance_action_type",
    )
    .map(([key, value]) => {
      const label = key.charAt(0).toUpperCase() + key.slice(1).replace(/_/g, " ");

      return (
        <Box sx={{ mb: 5, width: "100%" }}>
          <Typography color="neutralGray" fontWeight={400} variant="body2">
            {label}
          </Typography>
          <Typography
            sx={{ mt: 0.5, wordBreak: "break-word" }}
            variant="body2"
          >
            {value as string}
          </Typography>
        </Box>
      );
    });

  const renderLinks = () => {
    const links = values.links?.map((item) => item.link) ?? [];
    const areLinks = links.some((item) => item);

    return areLinks ? (
      <>
        <Typography
          color="neutralGray"
          fontWeight={400}
          sx={{ mb: 0.5 }}
          variant="body2"
        >
          {t("createGovernanceAction.supportingLinks")}
        </Typography>
        {links.map((link: string) => (link ? (
          <LinkWithIcon
            icon={<img src={ICONS.link} />}
            label={link}
            onClick={() => onClickLink(link)}
            sx={{ mb: 1.75 }}
          />
        ) : null))}
      </>
    ) : null;
  };

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBackButton}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.reviewSubmission")}
      </Typography>
      <Spacer y={4.25} />
      <Button
        startIcon={(
          <DriveFileRenameOutlineOutlinedIcon
            color="primary"
            fontSize="large"
          />
        )}
        onClick={onClickEditSubmission}
        sx={{ alignSelf: "center", width: "180px" }}
        variant="outlined"
      >
        {t("createGovernanceAction.editSubmission")}
      </Button>
      <Spacer y={6} />
      {renderReviewFields()}
      {renderLinks()}
      <Spacer y={6} />
    </BgCard>
  );
}
