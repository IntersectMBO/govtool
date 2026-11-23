// What the proposal details page shows, decided from the proposal, the
// signed-in forum user and their sentiment vote. Kept out of the page so
// each decision can be tested on its own.

// The side column's first card. Once submitted, the proposal links to its
// vote, or to its outcome when the vote has ended; before that its author
// may submit it, and everyone else is asked to comment. Submission needs
// prop_submitted to be exactly false, as the page always required.
export const proposalActionKind = ({
    submitted,
    hasUser,
    isAuthor,
    voteEnded,
}) => {
    if (submitted) return voteEnded ? 'outcome' : 'vote';
    if (hasUser && submitted === false && isAuthor) return 'submit';
    return 'discuss';
};

// The status pill, worded and coloured as on the proposals list.
export const proposalStatus = (submitted) =>
    submitted
        ? { label: 'Submitted for vote', tone: 'primary' }
        : { label: 'Active', tone: 'success' };

// A count as the page's old badges showed it: MUI's Badge caps at 99+.
export const cappedCount = (value) => {
    const count = Number(value) || 0;
    return count > 99 ? '99+' : String(count);
};

// Which type-specific details card the proposal has, if any. Type ids are
// the forum's: 2 treasury withdrawals, 3 constitution, 6 hard fork.
export const typeDetailsKind = (content) => {
    const typeId = content?.gov_action_type_id;
    if (typeId == 2) {
        return content?.proposal_withdrawals?.length > 0 ? 'withdrawals' : null;
    }
    if (typeId == 3) {
        return content?.proposal_constitution_content?.data
            ? 'constitution'
            : null;
    }
    if (typeId == 6) return 'hardFork';
    return null;
};

// The like or dislike button: its tooltip, whether it is disabled, what a
// click does ('username' asks for a display name, 'vote' records the
// sentiment, null does nothing) and whether it shows the user's own vote.
export const sentimentButton = ({
    like,
    submitted,
    hasWallet,
    user,
    isAuthor,
    vote,
}) => {
    const verb = like ? 'like' : 'dislike';
    const Verb = like ? 'Like' : 'Dislike';
    const isOwnVote = vote?.attributes?.vote_result === like;

    let tooltip;
    if (submitted) tooltip = `Proposal Submitted\n\nYou can't ${verb} this proposal`;
    else if (!hasWallet) tooltip = `Connect wallet to ${verb} this proposal`;
    else if (!user) tooltip = `Verify yourself to ${verb} this proposal`;
    else if (isAuthor) tooltip = `You can't ${verb} your proposal`;
    else if (vote && isOwnVote) tooltip = `You already ${verb}d this proposal`;
    else tooltip = `${Verb} this proposal\n\nClick to ${verb} this proposal`;

    let disabled;
    if (!hasWallet || submitted) disabled = true;
    else if (!user) disabled = false;
    else if (isAuthor) disabled = true;
    else disabled = Boolean(vote) && isOwnVote;

    let action;
    if (submitted) action = null;
    else if (!user) action = 'vote';
    else if (!user?.user?.govtool_username) action = 'username';
    else if (isAuthor) action = null;
    else if (vote && vote?.attributes?.vote_result === null) action = null;
    else action = 'vote';

    return {
        tooltip,
        disabled,
        action,
        active: Boolean(user) && Boolean(vote) && isOwnVote,
    };
};
