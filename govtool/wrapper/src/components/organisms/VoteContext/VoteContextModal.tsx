import { useState } from "react";
import { useForm, FormProvider } from "react-hook-form";

import { ModalWrapper } from "@atoms";
import { useModal } from "@context";
import {
  VoteContextStoringInformation,
  VoteContextCheckResult,
  VoteContextTerms,
  VoteContextText,
} from "@organisms";
import { VoteContextFormValues } from "@hooks";

type VoteContextModalState = {
  onSubmit: (url: string, hash: string | null, voteContextText: string) => void;
};

export const VoteContextModal = () => {
  const [step, setStep] = useState(1);
  const [savedHash, setSavedHash] = useState<string | null>("");
  const [errorMessage, setErrorMessage] = useState<string | undefined>(
    undefined,
  );

  const { state, closeModal } = useModal<VoteContextModalState>();

  const methods = useForm<VoteContextFormValues>({ mode: "onChange" });
  const { getValues } = methods;

  const submitVoteContext = () => {
    if (state && savedHash) {
      state.onSubmit(
        getValues("storingURL"),
        savedHash,
        getValues("voteContextText"),
      );
    }
    closeModal();
  };

  return (
    <ModalWrapper
      dataTestId="vote-context-modal"
      sx={{
        maxWidth: "700px",
        p: "60px 32px 32px",
      }}
    >
      <FormProvider {...methods}>
        {step === 1 && (
          <VoteContextText setStep={setStep} onCancel={closeModal} />
        )}
        {step === 2 && (
          <VoteContextTerms setStep={setStep} onCancel={closeModal} />
        )}
        {step === 3 && (
          <VoteContextStoringInformation
            setSavedHash={setSavedHash}
            setStep={setStep}
            setErrorMessage={setErrorMessage}
            onCancel={closeModal}
          />
        )}
        {step === 4 && (
          <VoteContextCheckResult
            submitVoteContext={submitVoteContext}
            closeModal={closeModal}
            setStep={setStep}
            errorMessage={errorMessage}
          />
        )}
      </FormProvider>
    </ModalWrapper>
  );
};
