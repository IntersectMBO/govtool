import { Dispatch, SetStateAction, useCallback } from 'react';
import { Box } from '@mui/material';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';

import { Button, Spacer, Typography } from '@atoms';
import { ICONS } from '@consts';
import {
  useCreateGovernanceActionForm,
  useTranslation,
  useScreenDimension,
} from '@hooks';
import { Step } from '@molecules';
import { BgCard, ControlledField } from '@organisms';
import { URL_REGEX, openInNewTab } from '@utils';

type StorageInformationProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const StorageInformation = ({ setStep }: StorageInformationProps) => {
  const { t } = useTranslation();
  const {
    control,
    errors,
    createGovernanceAction,
    getValues,
    watch,
    onClickDownloadJson,
    isLoading,
  } = useCreateGovernanceActionForm(setStep);
  const { screenWidth } = useScreenDimension();

  // TODO: change on correct file name
  const fileName = getValues('governance_action_type');

  // TODO: Change link to correct
  const openGuideAboutStoringInformation = useCallback(
    () => openInNewTab('https://sancho.network/'),
    [],
  );

  const isActionButtonDisabled = !watch('storingURL');

  const onClickBack = useCallback(() => setStep(5), []);

  return (
    <BgCard
      actionButtonLabel={t('continue')}
      backButtonLabel={t('back')}
      isActionButtonDisabled={isActionButtonDisabled}
      onClickActionButton={createGovernanceAction}
      onClickBackButton={onClickBack}
      isLoadingActionButton={isLoading}
    >
      <Typography sx={{ textAlign: 'center' }} variant="headline4">
        {t('createGovernanceAction.storingInformationTitle')}
      </Typography>
      <Button
        endIcon={
          <OpenInNewIcon
            sx={{
              color: 'primary',
              height: 17,
              width: 17,
            }}
          />
        }
        onClick={openGuideAboutStoringInformation}
        size="extraLarge"
        sx={{ alignSelf: 'center', width: 'fit-content' }}
        variant="text"
      >
        {t('createGovernanceAction.storingInformationStep2Link')}
      </Button>
      <Typography fontWeight={400} sx={{ textAlign: 'center' }} variant="body1">
        {t('createGovernanceAction.storingInformationDescription')}
      </Typography>
      <Box sx={{ my: 4 }}>
        <Step
          component={
            <Button
              onClick={onClickDownloadJson}
              size="extraLarge"
              startIcon={<img alt="download" src={ICONS.download} />}
              sx={{
                width: 'fit-content',
                ml: screenWidth < 1024 ? 0 : 1.75,
                mt: screenWidth < 1024 ? 1.5 : 0,
              }}
              variant="outlined"
            >
              {`${fileName}.jsonld`}
            </Button>
          }
          componentsLayoutStyles={{
            alignItems: screenWidth < 1024 ? undefined : 'center',
            flexDirection: screenWidth < 1024 ? 'column' : 'row',
          }}
          label={t('createGovernanceAction.storingInformationStep1Label')}
          stepNumber={1}
        />
        <Spacer y={6} />
        <Step
          component={
            <Button
              endIcon={
                <OpenInNewIcon
                  sx={{
                    color: "primary",
                    height: 17,
                    width: 17,
                  }}
                />
              }
              onClick={openGuideAboutStoringInformation}
              size="extraLarge"
              sx={{ width: "fit-content" }}
              variant="text"
            >
              {t("createGovernanceAction.storingInformationStep2Link")}
            </Button>
          }
          label={t('createGovernanceAction.storingInformationStep2Label')}
          stepNumber={2}
        />
        <Spacer y={6} />
        <Step
          component={
            <ControlledField.Input
              {...{ control, errors }}
              name="storingURL"
              layoutStyles={{ mt: 1.5 }}
              placeholder={t(
                'createGovernanceAction.storingInformationURLPlaceholder',
              )}
              rules={{
                required: {
                  value: true,
                  message: t(
                    'createGovernanceAction.fields.validations.required',
                  ),
                },
                pattern: {
                  value: URL_REGEX,
                  message: t('createGovernanceAction.fields.validations.url'),
                },
              }}
            />
          }
          label={t('createGovernanceAction.storingInformationStep3Label')}
          stepNumber={3}
        />
      </Box>
    </BgCard>
  );
};
