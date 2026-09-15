import { useTranslation } from "react-i18next";

import { correctVoteAdaFormat } from "@utils";
import { GovernanceActionCardElement } from "./GovernanceActionCardElement";

type Props = {
  receivingAddress: string;
  amount: number;
};

export const GovernanceActionCardTreasuryWithdrawalElement = ({
  receivingAddress,
  amount,
}: Props) => {
  const { t } = useTranslation();

  return (
    <>
      <GovernanceActionCardElement
        label={t("govActions.receivingAddress")}
        text={receivingAddress}
        textVariant="oneLine"
        dataTestId="receiving-address-label"
        isCopyButton
      />
      <GovernanceActionCardElement
        label={t("govActions.amount")}
        text={`₳ ${correctVoteAdaFormat(amount) ?? 0}`}
        textVariant="oneLine"
        dataTestId="amount"
      />
    </>
  );
};
