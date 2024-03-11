import { useEffect } from "react";
import { Meta, StoryFn } from "@storybook/react";
import { expect, jest } from "@storybook/jest";
import { userEvent, waitFor, within, screen } from "@storybook/testing-library";

import { Modal } from "@atoms";
import { StatusModal, StatusModalState } from "@organisms";
import { useModal } from "../../context/modal";
import { callAll } from "@utils";

const meta = {
  title: "Example/Modals/StatusModalWithLink",
  component: StatusModal,
} satisfies Meta<typeof StatusModal>;

export default meta;

const Template: StoryFn<StatusModalState> = (args) => {
  const { openModal, modal, modals, closeModal } = useModal();

  const open = () => {
    openModal({
      type: "statusModal",
      state: {
        buttonText: "Close",
        status: "success",
        onSubmit: () => closeModal(),
        title: args.title,
        message: args.message,
        link: args.link,
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

export const Default = Template.bind({});
Default.args = {
  status: "success",
  message: "Example text",
  title: "Success",
  link: "examplelink.com",
  dataTestId: "status-modal",
};

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("status-modal")[0];
    let modalCanvas = within(modalScreen);
    window.open = jest.fn();

    expect(modalCanvas.getByText("Success")).toBeInTheDocument();
    expect(modalCanvas.getByText("Example text")).toBeInTheDocument();

    await userEvent.click(modalCanvas.getByText(/this link/i));
    expect(window.open).toBeCalledTimes(1);
    expect(screen.queryAllByTestId("status-modal")).toHaveLength(0); // checking id modal is closed

    // Validating closing of modal
    await userEvent.click(canvas.getByRole("button"));
    modalCanvas = within(screen.getAllByTestId("status-modal")[0]);
    await userEvent.click(modalCanvas.getByTestId("cancel-modal-button"));
    expect(screen.queryAllByTestId("external-link-modal")).toHaveLength(0); // checking id modal is closed

    await userEvent.click(canvas.getByRole("button"));
    modalCanvas = within(screen.getAllByTestId("status-modal")[0]);
    await userEvent.click(modalCanvas.getByTestId("confirm-modal-button"));
    expect(screen.queryAllByTestId("external-link-modal")).toHaveLength(0); // checking id modal is closed
  });
};
