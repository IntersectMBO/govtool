import { Meta, StoryFn } from "@storybook/react";

import { Modal } from "@atoms";
import { StatusModal, VotingPowerModalState } from "@organisms";
import { useModal } from "@context";
import { callAll } from "@utils";

const meta = {
  title: "Example/Modals/VotingPowerModal",
  component: StatusModal,
} satisfies Meta<typeof StatusModal>;

export default meta;

const Template: StoryFn<VotingPowerModalState> = (args) => {
  const { openModal, modal, modals } = useModal();

  const open = () => {
    openModal({
      type: "votingPower",
      state: {
        ...args,
      },
    });
  };

  return (
    <>
      <button onClick={open} style={{ cursor: "pointer" }}>
        Open Modal
      </button>
      {modals[modal.type]?.component && (
        <Modal
          open={Boolean(modals[modal.type].component)}
          handleClose={callAll(modals[modal.type]?.onClose, () =>
            openModal({ type: "none", state: null })
          )}
        >
          {modals[modal.type]?.component ?? <></>}
        </Modal>
      )}
    </>
  );
};

export const Default = Template.bind({});
Default.args = {
  yesVotes: 1000000000000,
  noVotes: 10000000000,
  abstainVotes: 324000000,
  vote: "yes",
};
