import { useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";

export type RegisterAsDRepValues = {
  bio?: string;
  dRepName: string;
  email?: string;
  links?: { link: string }[];
  storeData?: boolean;
  storingURL: string;
};

export const defaultRegisterAsDRepValues: RegisterAsDRepValues = {
  bio: "",
  dRepName: "",
  email: "",
  links: [{ link: "" }],
  storeData: false,
  storingURL: "",
};

export const useRegisterAsdRepForm = () => {
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const {
    control,
    handleSubmit,
    formState: { errors, isValid },
    register,
    resetField,
    watch,
  } = useFormContext<RegisterAsDRepValues>();

  const onSubmit = useCallback(async (values: RegisterAsDRepValues) => {
    try {
      // eslint-disable-next-line no-console
      console.log(values);
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (e: any) {
      console.error(e);
    } finally {
      setIsLoading(false);
    }
  }, []);

  return {
    control,
    errors,
    isRegistrationAsDRepLoading: isLoading,
    isValid,
    register,
    registerAsDrep: handleSubmit(onSubmit),
    resetField,
    watch,
  };
};
