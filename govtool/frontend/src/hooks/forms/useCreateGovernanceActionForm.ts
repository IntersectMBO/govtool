import {
  GovernanceActionFieldSchemas,
  GovernanceActionType,
} from "@/types/governanceAction";
import { useCallback, useState } from "react";
import { useFormContext } from "react-hook-form";

import { CIP_100, CIP_108, GOVERNANCE_ACTION_CONTEXTS } from "@consts";

import * as jsonld from "jsonld";

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

  const govActionType = watch("governance_action_type");

  // TODO: To be moved to utils
  const generateJsonBody = async (data: CreateGovernanceActionValues) => {
    const filteredData = Object.entries(data)
      .filter(
        ([key]) =>
          !Object.keys(defaulCreateGovernanceActionValues).includes(key) &&
          key !== "governance_action_type"
      )
      .map(([key, value]) => {
        return [CIP_108 + key, value];
      });

    const references = (data as CreateGovernanceActionValues).links
      ?.filter((link) => link.link)
      .map((link) => {
        return {
          [`@type`]: "Other",
          [`${CIP_100}reference-label`]: "Label",
          [`${CIP_100}reference-uri`]: link.link,
        };
      });

    const body = {
      ...Object.fromEntries(filteredData),
      [`${CIP_108}references`]: references,
    };

    const doc = {
      [`${CIP_108}body`]: body,
      [`${CIP_100}hashAlgorithm`]: "blake2b-256",
      [`${CIP_100}authors`]: [],
    };

    const json = await jsonld.compact(
      doc,
      GOVERNANCE_ACTION_CONTEXTS[govActionType as GovernanceActionType]
    );

    return JSON.stringify(json);
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
