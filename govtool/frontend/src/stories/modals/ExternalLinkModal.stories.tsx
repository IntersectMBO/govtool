import { useEffect } from "react";
import { Meta, StoryFn } from "@storybook/react";
import {
  expect,
  userEvent,
  within,
  screen,
  waitFor,
  fn,
} from "@storybook/test";

import { Modal } from "@atoms";
import { ExternalLinkModal, ExternalLinkModalState } from "@organisms";
import { callAll } from "@/utils";

import { useModal } from "../../context/modal";

const meta = {
  title: "Example/Modals/ExternalLinkModal",
  component: ExternalLinkModal,
} satisfies Meta<typeof ExternalLinkModal>;

export default meta;

const Template: StoryFn<ExternalLinkModalState> = ({ externalLink }) => {
  const { openModal, modal, modals } = useModal();

  const open = () => {
    openModal({
      type: "externalLink",
      state: { externalLink },
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
  externalLink: "https://www.google.com/",
};

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("external-link-modal")[0];
    let modalCanvas = within(modalScreen);
    window.open = fn();

    expect(modalCanvas.getByText(/Be Careful!/i)).toBeInTheDocument();
    expect(
      modalCanvas.getByText("https://www.google.com/"),
    ).toBeInTheDocument();

    await userEvent.click(modalCanvas.getByTestId("continue-modal-button"));
    expect(screen.queryAllByTestId("external-link-modal")).toHaveLength(0); // checking id modal is closed
    expect(window.open).toBeCalledTimes(1);
    expect(screen.queryAllByTestId("external-link-modal")).toHaveLength(0); // checking id modal is closed

    // Validating close modal
    await userEvent.click(canvas.getByRole("button"));
    modalCanvas = within(screen.getAllByTestId("external-link-modal")[0]);
    await userEvent.click(modalCanvas.getByTestId("cancel-modal-button"));
    expect(screen.queryAllByTestId("external-link-modal")).toHaveLength(0); // checking id modal is closed

    await userEvent.click(canvas.getByRole("button"));
    modalCanvas = within(screen.getAllByTestId("external-link-modal")[0]);
    await userEvent.click(modalCanvas.getByTestId("close-modal-button"));
    expect(screen.queryAllByTestId("external-link-modal")).toHaveLength(0); // checking id modal is closed
  });
};
