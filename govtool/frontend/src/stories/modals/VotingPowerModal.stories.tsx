import { expect } from "@storybook/jest";
import { Meta, StoryFn } from "@storybook/react";

import { Modal } from "@atoms";
import { useModal } from "@context";
import { StatusModal, VotingPowerModalState } from "@organisms";
import { screen, waitFor, within } from "@storybook/testing-library";
import { callAll, correctAdaFormat } from "@utils";
import { useEffect } from "react";

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

async function assertVotes(
  canvas: ReturnType<typeof within>,
  args: VotingPowerModalState,
) {
  const yesVotesText = `₳ ${correctAdaFormat(args.yesVotes)}`;
  const noVotesText = `₳ ${correctAdaFormat(args.noVotes)}`;
  const abstainVotesText = `₳ ${correctAdaFormat(args.abstainVotes)}`;

  await expect(canvas.getByText(yesVotesText)).toBeVisible();
  await expect(canvas.getByText(noVotesText)).toBeVisible();
  await expect(canvas.getByText(abstainVotesText)).toBeVisible();
}

export const YesVoted = Template.bind({});
YesVoted.args = {
  yesVotes: 1000000000000,
  noVotes: 10000000000,
  abstainVotes: 324000000,
  vote: "yes",
};
YesVoted.play = async ({ args }) => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("external-link-modal")[0];
    const loadingModalCanvas = within(modalScreen);

    await assertVotes(loadingModalCanvas, args);
  });
};

export const AbstainVoted = Template.bind({});
AbstainVoted.args = {
  yesVotes: 1000000000000,
  noVotes: 10000000000,
  abstainVotes: 324000000,
  vote: "abstain",
};
AbstainVoted.play = async ({ args }) => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("external-link-modal")[0];
    const loadingModalCanvas = within(modalScreen);

    await assertVotes(loadingModalCanvas, args);
  });
};

export const NoVoted = Template.bind({});
NoVoted.args = {
  yesVotes: 1000000000000,
  noVotes: 10000000000,
  abstainVotes: 324000000,
  vote: "no",
};
NoVoted.play = async ({ args }) => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("external-link-modal")[0];
    const loadingModalCanvas = within(modalScreen);

    await assertVotes(loadingModalCanvas, args);
  });
};
