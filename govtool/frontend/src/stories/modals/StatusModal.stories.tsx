import { useEffect } from "react";
import { Meta, StoryFn } from "@storybook/react";
import { expect } from "@storybook/jest";
import { within, waitFor, screen, userEvent } from "@storybook/testing-library";

import { Modal } from "@atoms";
import { StatusModal, StatusModalState } from "@organisms";
import { useModal } from "../../context/modal";
import { callAll } from "@utils";

const meta = {
  title: "Example/Modals/StatusModal",
  component: StatusModal,
} satisfies Meta<typeof StatusModal>;

export default meta;

const performCommonAction = async (canvas: any, args: any) => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId(args.dataTestId)[0];
    let modalCanvas = within(modalScreen);

    expect(modalCanvas.getByText(args.title)).toBeInTheDocument();
    expect(modalCanvas.getByText(args.message)).toBeInTheDocument();

    // Validating closing of modal
    await userEvent.click(modalCanvas.getByTestId("confirm-modal-button"));
    expect(screen.queryAllByTestId(args.dataTestId)).toHaveLength(0); // checking id modal is closed

    await userEvent.click(canvas.getByRole("button"));
    modalCanvas = within(screen.getAllByTestId(args.dataTestId)[0]);
    await userEvent.click(modalCanvas.getByTestId("close-modal-button"));
    expect(screen.queryAllByTestId(args.dataTestId)).toHaveLength(0); // checking id modal is closed
  });
};
const Template: StoryFn<StatusModalState> = (args) => {
  const { openModal, modal, modals, closeModal } = useModal();

  const open = () => {
    openModal({
      type: "statusModal",
      state: {
        buttonText: args.buttonText,
        status: args.status,
        message: args.message,
        title: args.title,
        onSubmit: () => {
          closeModal();
        },
        dataTestId: args.dataTestId,
      },
    });
  };

  useEffect(() => {
    open();
  }, [openModal]);

  return (
    <>
      <button onClick={open} style={{ cursor: "pointer" }}>
        Open Modal
      </button>
      {modals[modal.type]?.component && (
        <Modal
          open={Boolean(modals[modal.type].component)}
          handleClose={
            !modals[modal.type].preventDismiss
              ? callAll(modals[modal.type]?.onClose, () =>
                  openModal({ type: "none", state: null })
                )
              : undefined
          }
        >
          {modals[modal.type]?.component ?? <></>}
        </Modal>
      )}
    </>
  );
};

export const Warning = Template.bind({});
Warning.args = {
  status: "warning",
  message:
    "We had trouble processing your request. Please try again or come back later.",
  title: "Oops!",
  dataTestId: "warning-modal",
};

Warning.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);
  await performCommonAction(canvas, args);
};

export const Success = Template.bind({});
Success.args = {
  status: "success",
  message: "Delegation transaction submitted!",
  title: "Success!",
  dataTestId: "status-modal",
};

Success.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);
  await performCommonAction(canvas, args);
};

export const Info = Template.bind({});
Info.args = {
  status: "info",
  message:
    "Before performing a new action please wait for the previous action transaction to be completed.",
  title: "Please wait for your previous transaction to be completed.",
  dataTestId: "info-modal",
  buttonText: "Ok",
};

Info.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);
  await performCommonAction(canvas, args);
};
