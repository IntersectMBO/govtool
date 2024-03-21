import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { FormProvider, useForm } from 'react-hook-form';
import { Box } from '@mui/material';

import { Background } from '@atoms';
import { PATHS } from '@consts';
import { useModal } from '@context';
import {
  defaulCreateGovernanceActionValues,
  useScreenDimension,
  useTranslation,
} from '@hooks';
import { LinkWithIcon } from '@molecules';
import {
  ChooseGovernanceActionType,
  CreateGovernanceActionForm,
  DashboardTopNav,
  Footer,
  ReviewCreatedGovernanceAction,
  StorageInformation,
  StoreDataInfo,
  WhatGovernanceActionIsAbout,
} from '@organisms';
import { checkIsWalletConnected } from '@utils';

export const CreateGovernanceAction = () => {
  const [step, setStep] = useState(1);
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { closeModal, openModal } = useModal();

  const methods = useForm({
    mode: 'onChange',
    defaultValues: defaulCreateGovernanceActionValues,
  });

  useEffect(() => {
    if (checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const backToDashboard = () => {
    navigate(PATHS.dashboard);
    closeModal();
  };

  const onClickBackToDashboard = () =>
    openModal({
      type: 'statusModal',
      state: {
        status: 'warning',
        message: t('modals.createGovernanceAction.cancelModalDescription'),
        buttonText: t('modals.common.goToDashboard'),
        title: t('modals.createGovernanceAction.cancelModalTitle'),
        dataTestId: 'cancel-governance-action-creation-modal',
        onSubmit: backToDashboard,
      },
    });

  return (
    <Background isReverted>
      <Box
        sx={{ display: 'flex', flexDirection: 'column', minHeight: '100vh' }}
      >
        <DashboardTopNav title={t('createGovernanceAction.title')} />
        <LinkWithIcon
          label={t('backToDashboard')}
          onClick={onClickBackToDashboard}
          sx={{
            mb: isMobile ? 0 : 1.5,
            ml: isMobile ? 2 : 5,
            mt: isMobile ? 3 : 1.5,
          }}
        />
        <FormProvider {...methods}>
          {step === 1 && (
            <WhatGovernanceActionIsAbout
              onClickCancel={onClickBackToDashboard}
              setStep={setStep}
            />
          )}
          {step === 2 && <ChooseGovernanceActionType setStep={setStep} />}
          {step === 3 && <CreateGovernanceActionForm setStep={setStep} />}
          {step === 4 && <ReviewCreatedGovernanceAction setStep={setStep} />}
          {step === 5 && <StoreDataInfo setStep={setStep} />}
          {step === 6 && <StorageInformation setStep={setStep} />}
        </FormProvider>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
