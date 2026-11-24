import { useState } from "react";
import { FormProvider, useForm } from "react-hook-form";
import { useTranslation } from "react-i18next";
import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent, waitFor, within } from "storybook/test";

import { CenteredBoxPageWrapper } from "@molecules";
import {
  ChooseGovernanceActionType,
  CreateGovernanceActionForm,
  ReviewCreatedGovernanceAction,
  StorageInformation,
  StoreDataInfo,
  WhatGovernanceActionIsAbout,
} from "@organisms";
import {
  defaulCreateGovernanceActionValues,
  type CreateGovernanceActionValues,
} from "@/hooks/forms/useCreateGovernanceActionForm";
import { GovernanceActionType } from "@/types/governanceAction";

type HarnessProps = {
  step: number;
  values?: Partial<CreateGovernanceActionValues>;
};

/** The create governance action page without its wallet redirect. */
const Harness = ({ step, values }: HarnessProps) => {
  const { t } = useTranslation();
  const [current, setStep] = useState(step);
  const methods = useForm<CreateGovernanceActionValues>({
    mode: "onChange",
    defaultValues: { ...defaulCreateGovernanceActionValues, ...values },
  });

  return (
    <CenteredBoxPageWrapper
      frameless
      pageTitle={t("createGovernanceAction.title")}
      stepper={{
        steps: [
          t("createGA.steps.understand"),
          t("createGA.steps.type"),
          t("createGA.steps.details"),
          t("createGA.steps.review"),
          t("createGA.steps.prepareFile"),
          t("createGA.steps.confirm"),
        ],
        current,
      }}
    >
      <FormProvider {...methods}>
        {current === 1 && (
          <WhatGovernanceActionIsAbout onClickCancel={() => {}} setStep={setStep} />
        )}
        {current === 2 && <ChooseGovernanceActionType setStep={setStep} />}
        {current === 3 && <CreateGovernanceActionForm setStep={setStep} />}
        {current === 4 && <ReviewCreatedGovernanceAction setStep={setStep} />}
        {current === 5 && <StoreDataInfo setStep={setStep} />}
        {current === 6 && <StorageInformation setStep={setStep} />}
      </FormProvider>
    </CenteredBoxPageWrapper>
  );
};

const TREASURY: Partial<CreateGovernanceActionValues> = {
  governance_action_type: GovernanceActionType.TreasuryWithdrawals,
  title: "Strike Finance Liquidity Deployment",
  abstract: "Deploy 9,000,000 ada of treasury capital into perpetual futures liquidity.",
  motivation: "Perpetual futures are one of the largest categories of trading activity.",
  rationale: "Market context\nTreasury-owned capital deepens Cardano-native liquidity.",
  receivingAddress: "stake1u8vadh7hzuganqempk7kqu7e9dktl6pappdg5clr8q5dn5c98w6yj",
  amount: "9000000",
  references: [{ uri: "https://docs.gov.tools/treasury-reporting-standard" }],
};

const meta = {
  title: "Example/CreateGovernanceAction",
  component: Harness,
  parameters: { layout: "fullscreen" },
} satisfies Meta<typeof Harness>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Understand: Story = {
  args: { step: 1 },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("flow-stepper")).toHaveTextContent("Step 1 of 6");
    await expect(canvas.getByTestId("create-ga-acknowledgement")).toBeVisible();
    await expect(canvas.getByTestId("continue-button")).toHaveTextContent("Accept & Continue");
  },
};

export const ActionType: Story = {
  args: { step: 2, values: { governance_action_type: GovernanceActionType.TreasuryWithdrawals } },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const treasury = canvas.getByTestId("TreasuryWithdrawals-radio");
    await expect(treasury).toHaveAttribute("aria-checked", "true");
    await expect(treasury).toHaveTextContent("Selected");

    await userEvent.type(canvas.getByRole("searchbox"), "hard fork");
    await expect(canvas.getAllByRole("radio")).toHaveLength(1);
    await userEvent.clear(canvas.getByRole("searchbox"));

    await userEvent.click(canvas.getByTestId("InfoAction-radio"));
    await expect(canvas.getByTestId("InfoAction-radio")).toHaveAttribute("aria-checked", "true");
    await expect(canvas.getByTestId("TreasuryWithdrawals-radio")).toHaveTextContent("Available");
  },
};

export const ActionDetails: Story = {
  args: { step: 3, values: TREASURY },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("create-ga-identity")).toHaveTextContent("Required");
    await expect(canvas.getByTestId("create-ga-parameters")).toHaveTextContent("Address");
    await expect(canvas.getByTestId("continue-button")).toHaveTextContent("Continue & Preview");
    await userEvent.click(canvas.getByTestId("add-link-button"));
    await waitFor(() => expect(canvas.getByTestId("delete-link-2-button")).toBeVisible());
  },
};

export const Review: Story = {
  args: { step: 4, values: TREASURY },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("governance-action-title-content")).toHaveTextContent(
      "Strike Finance Liquidity Deployment",
    );
    await expect(canvas.getByTestId("create-ga-review")).toHaveTextContent("Treasury requests");
    await expect(canvas.getByTestId("governance-action-link-1-content")).toBeVisible();
    await expect(canvas.getByTestId("back-button")).toHaveTextContent("Back to Edit");
  },
};

export const PrepareFile: Story = {
  args: { step: 5, values: TREASURY },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("continue-button")).toBeDisabled();
    await userEvent.click(within(canvas.getByTestId("storing-information-checkbox")).getByRole("checkbox"));
    await expect(canvas.getByTestId("continue-button")).toBeEnabled();
  },
};

export const Confirm: Story = {
  args: { step: 6, values: { ...TREASURY, storeData: true } },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("create-ga-summary")).toHaveTextContent(
      "Strike Finance Liquidity Deployment",
    );
    await expect(canvas.getByTestId("metadata-download-button")).toBeVisible();
    await expect(canvas.getByTestId("create-ga-wallet")).toHaveTextContent("Connected");
    await expect(canvas.getByTestId("continue-button")).toHaveTextContent("Continue to wallet");
    await expect(canvas.getByTestId("continue-button")).toBeDisabled();
  },
};
