import { useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";

type createGovernanceActionValues = {
  type: string;
};

export const defaulCreateGovernanceActionValues: createGovernanceActionValues =
  {
    type: "",
  };

export const useCreateGovernanceActionForm = () => {
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const {
    control,
    formState: { errors, isValid },
    getValues,
    handleSubmit,
    setValue,
    watch,
  } = useFormContext<createGovernanceActionValues>();

  const onSubmit = useCallback(async () => {
    setIsLoading(true);
    try {
    } catch (e: any) {
    } finally {
      setIsLoading(false);
    }
  }, []);

  return {
    control,
    errors,
    getValues,
    isLoading,
    isValid,
    setValue,
    submitForm: handleSubmit(onSubmit),
    watch,
  };
};
