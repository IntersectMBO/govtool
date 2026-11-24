import { useSearchParams } from "react-router";

import { GOVERNANCE_ACTION_TYPE_PARAM } from "@consts";
import { DashboardGovernanceActions } from "@organisms";

import { DashboardGovernanceActionsCategory } from "./DashboardGovernanceActionsCategory";
import { GovernanceActions } from "./GovernanceActions";
import { GovernanceActionsCategory } from "./GovernanceActionsCategory";

type GovernanceActionsIndexProps = {
  isConnected?: boolean;
};

// /actions lists every live action, /actions?type=… one type's.
export const GovernanceActionsIndex = ({
  isConnected = false,
}: GovernanceActionsIndexProps) => {
  const [searchParams] = useSearchParams();

  if (searchParams.get(GOVERNANCE_ACTION_TYPE_PARAM)) {
    return isConnected ? (
      <DashboardGovernanceActionsCategory />
    ) : (
      <GovernanceActionsCategory />
    );
  }

  return isConnected ? <DashboardGovernanceActions /> : <GovernanceActions />;
};
