import { ReactNode } from "react";
import { act, fireEvent, render, screen } from "@testing-library/react";
import { FormProvider, useForm, useFormContext } from "react-hook-form";
import { MemoryRouter } from "react-router";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { EditDRepForm } from "./EditDRepForm";

const details = vi.hoisted(() => ({
  current: { dRep: undefined as unknown, isLoading: true },
}));

vi.mock("@context", () => ({ useCardano: () => ({ dRepID: "drep-id" }) }));

vi.mock("@hooks", () => ({
  defaultEditDRepInfoValues: {
    givenName: "",
    objectives: "",
    motivations: "",
    qualifications: "",
    paymentAddress: "",
    image: "",
    linkReferences: [],
    identityReferences: [],
  },
  useEditDRepInfoForm: () => {
    const { control, formState, register, reset, watch } = useFormContext();
    return {
      control,
      errors: formState.errors,
      isError: false,
      register,
      reset,
      watch,
    };
  },
  useGetDRepDetailsQuery: () => details.current,
  useTranslation: () => ({ t: (key: string) => key }),
}));

// The real form renders every CIP-119 field; one bound input is enough to see
// whether the stored metadata overwrote what was typed.
vi.mock("@molecules", () => ({
  DRepDataForm: ({ register }: { register: (name: string) => object }) => (
    <input data-testid="name-input" {...register("givenName")} />
  ),
  CenteredBoxBottomButtons: () => null,
}));

const Harness = ({ children }: { children: ReactNode }) => {
  const methods = useForm({ defaultValues: { givenName: "" } });
  return <FormProvider {...methods}>{children}</FormProvider>;
};

const tree = (loadUserData = true) => (
  <MemoryRouter>
    <Harness>
      <EditDRepForm
        onClickCancel={() => {}}
        setStep={() => {}}
        loadUserData={loadUserData}
        setLoadUserData={() => {}}
      />
    </Harness>
  </MemoryRouter>
);

const renderForm = (loadUserData = true) => render(tree(loadUserData));

const nameInput = () => screen.getByTestId("name-input") as HTMLInputElement;

describe("EditDRepForm prefill", () => {
  beforeEach(() => {
    details.current = { dRep: undefined, isLoading: true };
  });

  it("shows no form until the stored metadata has loaded, then prefills it", () => {
    const view = renderForm();
    expect(screen.queryByTestId("name-input")).toBeNull();
    expect(screen.getByRole("progressbar")).toBeTruthy();

    details.current = { dRep: { givenName: "Claude" }, isLoading: false };
    view.rerender(tree());
    expect(nameInput().value).toBe("Claude");
  });

  it("never overwrites what the user typed when the query data changes later", async () => {
    details.current = { dRep: { givenName: "Claude" }, isLoading: false };
    const view = renderForm();
    expect(nameInput().value).toBe("Claude");

    fireEvent.change(nameInput(), { target: { value: "Typed by user" } });
    expect(nameInput().value).toBe("Typed by user");

    // A refetch or a late response hands the component a new object.
    details.current = { dRep: { givenName: "Claude" }, isLoading: false };
    await act(async () => {
      view.rerender(tree());
    });
    expect(nameInput().value).toBe("Typed by user");
  });

  it("keeps the user's edits when coming back from the next step", () => {
    details.current = { dRep: undefined, isLoading: true };
    renderForm(false);
    // No spinner and no reset: step 1 was already prefilled once.
    expect(screen.getByTestId("name-input")).toBeTruthy();
    expect(nameInput().value).toBe("");
  });
});
