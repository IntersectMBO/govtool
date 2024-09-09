import { useEffect } from "react";
import type { Meta, StoryFn } from "@storybook/react";
import { expect, jest } from "@storybook/jest";
import { userEvent, waitFor, within, screen } from "@storybook/testing-library";

import { Modal } from "@atoms";
import { StatusModal, StatusModalState } from "@organisms";
import { callAll } from "@utils";
import { useModal } from "../../context/modal";

const meta = {
  title: "Example/Modals/StatusModalWithLink",
  component: StatusModal,
} satisfies Meta<typeof StatusModal>;

export default meta;

const Template: StoryFn<StatusModalState> = ({
  title,
  message,
  link,
  dataTestId,
}) => {
  const { openModal, modal, modals, closeModal } = useModal();

  const open = () => {
    openModal({
      type: "statusModal",
      state: {
        buttonText: "Close",
        status: "success",
        onSubmit: () => closeModal(),
        title,
        message,
        link,
        dataTestId,
      },
    });
  };

  useEffect(() => {
    open();
  }, [openModal]);

  return (
    <>
      <button type="button" onClick={open} style={{ cursor: "pointer" }}>
        Open Modal
      </button>
      {modals[modal.type]?.component && (
        <Modal
          open={Boolean(modals[modal.type].component)}
          handleClose={callAll(modals[modal.type]?.onClose, () =>
            openModal({ type: "none", state: null }),
          )}
        >
          {modals[modal.type].component!}
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

Default.play = async () => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("status-modal")[0];
    let modalCanvas = within(modalScreen);
    window.open = jest.fn();

    expect(modalCanvas.getByText("Success")).toBeInTheDocument();
    expect(modalCanvas.getByText("Example text")).toBeInTheDocument();

    await userEvent.click(modalCanvas.getByText(/this link/i));
    expect(window.open).toBeCalledTimes(1);
    expect(screen.queryAllByTestId("status-modal")).toHaveLength(1); // checking id modal is closed

    // Validating closing of modal
    modalCanvas = within(screen.getAllByTestId("status-modal")[0]);
    await userEvent.click(modalCanvas.getByTestId("confirm-modal-button"));
    expect(screen.queryAllByTestId("external-link-modal")).toHaveLength(0); // checking id modal is closed
  });
};
