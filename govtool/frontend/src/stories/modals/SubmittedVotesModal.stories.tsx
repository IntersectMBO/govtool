import { useEffect } from "react";
import { expect, screen, waitFor, within } from "@storybook/test";
import { Meta, StoryFn } from "@storybook/react";

import { Modal } from "@atoms";
import { useModal } from "@context";
import { StatusModal, SubmittedVotesModalState } from "@organisms";
import { callAll, correctAdaFormat } from "@utils";

const meta = {
  title: "Example/Modals/SubmittedVotesModal",
  component: StatusModal,
} satisfies Meta<typeof StatusModal>;

export default meta;

const Template: StoryFn<SubmittedVotesModalState> = (args) => {
  const { openModal, modal, modals } = useModal();

  const open = () => {
    openModal({
      type: "submittedVotes",
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
  args: SubmittedVotesModalState,
) {
  const dRepYesVotesText = `₳ ${correctAdaFormat(args.dRepYesVotes)}`;
  const dRepNoVotesText = `₳ ${correctAdaFormat(args.dRepNoVotes)}`;
  const dRepAbstainVotesText = `₳ ${correctAdaFormat(args.dRepAbstainVotes)}`;

  await expect(canvas.getByText(dRepYesVotesText)).toBeVisible();
  await expect(canvas.getByText(dRepNoVotesText)).toBeVisible();
  await expect(canvas.getByText(dRepAbstainVotesText)).toBeVisible();
}

export const YesVoted = Template.bind({});
YesVoted.args = {
  dRepYesVotes: 1000000000000,
  dRepNoVotes: 10000000000,
  dRepAbstainVotes: 324000000,
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
  dRepYesVotes: 1000000000000,
  dRepNoVotes: 10000000000,
  dRepAbstainVotes: 324000000,
  vote: "abstain",
};
AbstainVoted.play = async ({ args }) => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("submitted-votes-modal")[0];
    const loadingModalCanvas = within(modalScreen);

    await assertVotes(loadingModalCanvas, args);
  });
};

export const NoVoted = Template.bind({});
NoVoted.args = {
  dRepYesVotes: 1000000000000,
  dRepNoVotes: 10000000000,
  dRepAbstainVotes: 324000000,
  vote: "no",
};
NoVoted.play = async ({ args }) => {
  waitFor(async () => {
    const modalScreen = screen.getAllByTestId("submitted-votes-modal")[0];
    const loadingModalCanvas = within(modalScreen);

    await assertVotes(loadingModalCanvas, args);
  });
};
