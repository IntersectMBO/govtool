import {
  GovernanceActionFieldSchemas,
  GovernanceActionType,
} from "@/types/governanceAction";
import { useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";

export type CreateGovernanceActionValues = {
  links?: { link: string }[];
  storeData?: boolean;
  storingURL: string;
  governance_action_type?: GovernanceActionType;
} & Partial<Record<keyof GovernanceActionFieldSchemas, string>>;

export const defaulCreateGovernanceActionValues: CreateGovernanceActionValues =
  {
    links: [{ link: "" }],
    storeData: false,
    storingURL: "",
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
    register,
    reset,
  } = useFormContext<CreateGovernanceActionValues>();

  // TODO: To be moved to utils
  const generateJsonBody = (data: CreateGovernanceActionValues) => {
    const filteredData = Object.entries(data).filter(
      ([key]) => !Object.keys(defaulCreateGovernanceActionValues).includes(key)
    );
    const references = (data as CreateGovernanceActionValues).links
      ?.filter((link) => link.link)
      .map((link) => {
        // TODO: Label isnt available and hardcoded Other for type
        return { "@type": "Other", label: "Testlabel", uri: link.link };
      });
    const body = { ...Object.fromEntries(filteredData), references };

    const jsonStr = JSON.stringify(body);
    return jsonStr;
  };

  const onSubmit = useCallback(async (data: CreateGovernanceActionValues) => {
    try {
      setIsLoading(true);
      const jsonBody = generateJsonBody(data);

      return jsonBody;
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
    createGovernanceAction: handleSubmit(onSubmit),
    watch,
    register,
    reset,
    generateJsonBody,
  };
};
