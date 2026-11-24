import { ReactNode } from "react";
import { fireEvent, render, screen } from "@testing-library/react";
import { FormProvider, useForm, useFormContext } from "react-hook-form";
import { describe, expect, it, vi } from "vitest";

import { GovernanceActionType } from "@/types/governanceAction";

import { ChooseGovernanceActionType } from "./ChooseGovernanceActionType";

vi.mock("@/hooks/forms/useCreateGovernanceActionForm", () => ({
  useCreateGovernanceActionForm: () => {
    const { setValue, watch } = useFormContext();
    return { setValue, watch };
  },
}));

vi.mock("@/components/molecules/CenteredBoxBottomButtons", () => ({
  CenteredBoxBottomButtons: () => null,
}));

const Harness = ({ children, type }: { children: ReactNode; type?: GovernanceActionType }) => {
  const methods = useForm({ defaultValues: { governance_action_type: type } });
  return <FormProvider {...methods}>{children}</FormProvider>;
};

const renderPicker = (type?: GovernanceActionType) =>
  render(
    <Harness type={type}>
      <ChooseGovernanceActionType setStep={() => {}} />
    </Harness>,
  );

const tabbable = () =>
  screen.getAllByRole("radio").filter((radio) => radio.getAttribute("tabindex") === "0");

describe("ChooseGovernanceActionType", () => {
  it("lets Tab reach the selected type", () => {
    renderPicker(GovernanceActionType.InfoAction);
    expect(tabbable().map((radio) => radio.dataset.testid)).toEqual(["InfoAction-radio"]);
  });

  it("keeps one type reachable by Tab when the search hides the selected one", () => {
    renderPicker(GovernanceActionType.InfoAction);
    fireEvent.change(screen.getByRole("searchbox"), { target: { value: "treasury" } });
    expect(tabbable().map((radio) => radio.dataset.testid)).toEqual([
      "TreasuryWithdrawals-radio",
    ]);
  });

  it("selects the next type with the arrow keys", () => {
    renderPicker(GovernanceActionType.TreasuryWithdrawals);
    fireEvent.keyDown(screen.getByTestId("TreasuryWithdrawals-radio"), { key: "ArrowDown" });
    expect(screen.getByTestId("InfoAction-radio")).toHaveAttribute("aria-checked", "true");
  });

  it("lists the types in the design's order", () => {
    renderPicker();
    expect(screen.getAllByRole("radio").map((radio) => radio.dataset.testid)).toEqual([
      "TreasuryWithdrawals-radio",
      "InfoAction-radio",
      "HardForkInitiation-radio",
      "NewConstitution-radio",
      "NoConfidence-radio",
      "ParameterChange-radio",
      "NewCommittee-radio",
    ]);
  });
});
