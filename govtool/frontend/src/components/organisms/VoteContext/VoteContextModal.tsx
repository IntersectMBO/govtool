import { Dispatch, SetStateAction, useState } from "react";
import { useForm, FormProvider, UseFormReturn } from "react-hook-form";

import { ModalWrapper } from "@atoms";
import { useModal } from "@context";
import {
  VoteContextStoringInformation,
  VoteContextCheckResult,
  VoteContextTerms,
  VoteContextText,
  VoteContextChoice,
  VoteContextGovTool,
} from "@organisms";
import { NodeObject } from "jsonld";
import { VoteContextFormValues, useVoteContextForm } from "@hooks";
import { Vote } from "@/models";

export type VoteContextModalState = {
  onSubmit: (url: string, hash: string | null, voteContextText: string) => void;
  vote?: Vote;
  previousRationale?: string
  confirmVote: (
    vote?: Vote,
    url?: string,
    hash?: string | null,
  ) => void;
  // onRationaleChange : ()
};

export const VoteContextModal = () => {
  const [step, setStep] = useState(1);
  const [storeDataYourself, setStoreDataYourself] = useState(true);
  const [savedHash, setSavedHash] = useState<string | null>("");
  const [errorMessage, setErrorMessage] = useState<string | undefined>(
    undefined,
  );
  const [jsonldContent, setJsonldContent] = useState<NodeObject | null>(null);
  const [metadataHash, setMetadataHash] = useState<string | null>(null);

  const { state, closeModal } = useModal<VoteContextModalState>();

  const methods = useForm<VoteContextFormValues>({
    mode: "onChange",
    defaultValues: {
      voteContextText: state?.previousRationale || "",
    },
  });
  const { getValues } = methods;

  const submitVoteContext = () => {
    if (state) {
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
        {/* Call useVoteContextForm inside FormProvider */}
        {step !== 1 && (
          <VoteContextFlow
            step={step}
            setStep={setStep}
            storeDataYourself={storeDataYourself}
            setStoreDataYourself={setStoreDataYourself}
            setSavedHash={setSavedHash}
            setErrorMessage={setErrorMessage}
            jsonldContent={jsonldContent}
            setJsonldContent={setJsonldContent}
            metadataHash={metadataHash}
            setMetadataHash={setMetadataHash}
            submitVoteContext={submitVoteContext}
            onCancel={closeModal} // Pass closeModal directly
            errorMessage={errorMessage}
            methods={methods} // Pass methods to VoteContextFlow
          />
        )}
        {step === 1 && (
          <VoteContextText
            setStep={setStep}
            onCancel={closeModal}
            confirmVote={state?.confirmVote ?? (() => {})}
            vote={state?.vote}
            previousRationale={state?.previousRationale}
          />
        )}
      </FormProvider>
    </ModalWrapper>
  );
};

// New component to encapsulate the flow that uses useVoteContextForm
const VoteContextFlow = ({
  step,
  setStep,
  storeDataYourself,
  setStoreDataYourself,
  setSavedHash,
  setErrorMessage,
  jsonldContent,
  setJsonldContent,
  metadataHash,
  setMetadataHash,
  submitVoteContext,
  onCancel,
  errorMessage,
  methods, // Accept methods
}: {
  step: number;
  setStep: Dispatch<SetStateAction<number>>;
  storeDataYourself: boolean;
  setStoreDataYourself: Dispatch<SetStateAction<boolean>>;
  setSavedHash: Dispatch<SetStateAction<string | null>>;
  setErrorMessage: Dispatch<SetStateAction<string | undefined>>;
  jsonldContent: NodeObject | null;
  setJsonldContent: Dispatch<SetStateAction<NodeObject | null>>;
  metadataHash: string | null;
  setMetadataHash: Dispatch<SetStateAction<string | null>>;
  submitVoteContext: () => void;
  onCancel: () => void;
  errorMessage: string | undefined;
  methods: UseFormReturn<VoteContextFormValues>; // Type for methods
}) => {
  const { generateMetadata } = useVoteContextForm();

  return (
    <>
      {step === 2 && (
        <VoteContextChoice
          setStep={setStep}
          setStoreDataYourself={setStoreDataYourself}
          setJsonldContent={setJsonldContent}
          setMetadataHash={setMetadataHash}
          generateMetadata={generateMetadata}
        />
      )}
      {step === 3 && storeDataYourself && (
        <VoteContextTerms setStep={setStep} onCancel={onCancel} />
      )}
      {step === 3 && !storeDataYourself && (
        <VoteContextGovTool
          setStep={setStep}
          setSavedHash={setSavedHash}
          submitVoteContext={submitVoteContext}
          jsonldContent={jsonldContent}
          metadataHash={metadataHash}
          setValue={methods.setValue}
        />
      )}
      {step === 4 && (
        <VoteContextStoringInformation
          setSavedHash={setSavedHash}
          setStep={setStep}
          setErrorMessage={setErrorMessage}
          onCancel={onCancel}
        />
      )}
      {step === 5 && (
        <VoteContextCheckResult
          submitVoteContext={submitVoteContext}
          closeModal={onCancel}
          setStep={setStep}
          errorMessage={errorMessage}
        />
      )}
    </>
  );
};
