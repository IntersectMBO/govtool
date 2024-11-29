import { useEffect } from "react";
import { Meta, StoryFn } from "@storybook/react";
import { expect, screen, waitFor, within } from "@storybook/test";

import { Modal } from "@atoms";
import { LoadingModal, LoadingModalState } from "@organisms";
import { callAll } from "@utils";

import { useModal } from "../../context/modal";

const meta = {
  title: "Example/Modals/LoadingModal",
  component: LoadingModal,
} satisfies Meta<typeof LoadingModal>;

export default meta;

const Template: StoryFn<LoadingModalState> = ({
  message,
  title,
  dataTestId,
}) => {
  const { openModal, modal, modals, closeModal } = useModal();

  const open = () => {
    openModal({
      type: "loadingModal",
      state: {
        title,
        message,
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
      <button type="button" onClick={closeModal} style={{ cursor: "pointer" }}>
        Close Modal
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

export const Loading = Template.bind({});
Loading.args = {
  message:
    "GovTool will read the URL that you supplied and make a check to see if it’s identical with the information that you entered on the form.",
  title: "GovTool Is Checking Your Data",
  dataTestId: "loading-modal",
};
Loading.play = async ({ args }) => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("loading-modal")[0];
    const loadingModalCanvas = within(modalScreen);

    await expect(loadingModalCanvas.getByRole("img")).toHaveAttribute(
      "alt",
      "loader",
    );
    await expect(loadingModalCanvas.getByText(args.title)).toBeVisible();
    await expect(
      loadingModalCanvas.getByText(args.message as string),
    ).toBeVisible();
  });
};
