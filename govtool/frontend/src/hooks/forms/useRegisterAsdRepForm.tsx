import { useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";

type RegistrationAsDRepValues = {
  bio: string;
  dRepName: string;
  email: string;
  link1: string;
  storeData: boolean;
};

export const defaulDRepRegistrationtValues: RegistrationAsDRepValues = {
  bio: "",
  dRepName: "",
  email: "",
  link1: "",
  storeData: false,
};

export const useRegisterAsdRepForm = () => {
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const {
    control,
    handleSubmit,
    watch,
    formState: { errors, isValid },
  } = useFormContext<RegistrationAsDRepValues>();

  const onSubmit = useCallback(async (values: RegistrationAsDRepValues) => {
    setIsLoading(true);
    console.log(values);
    try {
    } catch (e: any) {
    } finally {
      setIsLoading(false);
    }
  }, []);

  return {
    control,
    errors,
    watch,
    isLoading,
    isValid,
    submitForm: handleSubmit(onSubmit),
  };
};
