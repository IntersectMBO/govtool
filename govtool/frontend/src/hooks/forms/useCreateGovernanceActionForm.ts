import { useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";

type createGovernanceActionValues = {
  governance_action_type: string;
  links?: { link: string }[];
  storeData?: boolean;
  storingURL: string;
};

export const defaulCreateGovernanceActionValues: createGovernanceActionValues =
  {
    governance_action_type: "",
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
  } = useFormContext<createGovernanceActionValues>();

  const generateJsonBody = (data: createGovernanceActionValues) => {
    const filteredData = Object.entries(data).filter(
      ([key]) => !Object.keys(defaulCreateGovernanceActionValues).includes(key)
    );
    const references = data.links
      ?.filter((link) => link.link)
      .map((link) => {
        // TODO: Label isnt available and harcoded Other for type
        return { "@type": "Other", label: "Testlabel", uri: link.link };
      });
    const body = { ...Object.fromEntries(filteredData), references };

    const jsonStr = JSON.stringify(body);
    return jsonStr;
  };

  const onSubmit = useCallback(async (data: createGovernanceActionValues) => {
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
