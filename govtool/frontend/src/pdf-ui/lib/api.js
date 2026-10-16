import axiosInstance, { axiosWithCookies } from './axiosInstance';

export const loginUser = async (loginData) => {
    try {
        const { data } = await axiosInstance.post(
            `/api/auth/local`,
            {
                ...loginData,
            },
            { withCredentials: true }
        );
        return data;
    } catch (error) {
        throw error;
    }
};
export const getProposals = async (query = '') => {
    try {
        const { data } = await axiosInstance.get(`/api/proposals?${query}`);

        const proposals = data?.data;
        const pgCount = data?.meta?.pagination?.pageCount;
        const total = data?.meta?.pagination?.total;
        return { proposals, pgCount, total };
    } catch (error) {
        return error;
    }
};
export const getSingleProposal = async (id) => {
    try {
        const { data } = await axiosInstance.get(`/api/proposals/${id}`);

        return data?.data;
    } catch (error) {
        throw error;
    }
};

export const updateProposalContent = async (id, proposalData) => {
    try {
        const { data } = await axiosInstance.put(
            `/api/proposal-contents/${id}`,
            {
                data: {
                    ...proposalData,
                },
            }
        );

        return data?.data;
    } catch (error) {
        console.error(error);
    }
};
export const getGovernanceActionTypes = async () => {
    try {
        const { data } = await axiosInstance.get(
            `/api/governance-action-types`
        );
        return data;
    } catch (error) {
        throw error;
    }
};

export const createProposal = async (data) => {
    try {
        const response = await axiosInstance.post(`/api/proposals`, {
            data: {
                ...data,
            },
        });

        return response?.data;
    } catch (error) {
        console.error('Error in createProposal:', error);
        throw error;
    }
};

export const createProposalContent = async (data, publish) => {
    try {
        const response = await axiosInstance.post(`/api/proposal-contents`, {
            data: {
                ...data,
                publish: publish,
            },
        });

        return response?.data;
    } catch (error) {
        console.error('Error in createProposal:', error);
        throw error;
    }
};

export const deleteProposal = async (proposalId) => {
    try {
        const response = await axiosInstance.delete(
            `/api/proposals/${proposalId}`
        );

        return response?.data;
    } catch (e) {
        console.error(e);
    }
};

export const getPolls = async ({ query = '' }) => {
    try {
        const { data } = await axiosInstance.get(`/api/polls?${query}`);

        const polls = data?.data;
        const pgCount = data?.meta?.pagination?.pageCount;
        const total = data?.meta?.pagination?.total;
        return { polls, pgCount, total };
    } catch (error) {
        throw error;
    }
};

export const createPoll = async ({ pollData }) => {
    try {
        const { data } = await axiosInstance.post(`/api/polls`, pollData);

        return data?.data;
    } catch (error) {
        throw error;
    }
};

export const getComments = async (query = '') => {
    try {
        const { data } = await axiosInstance.get(`api/comments?${query}`);
        const comments = data?.data;
        const pgCount = data?.meta?.pagination?.pageCount;
        const total = data?.meta?.pagination?.total;
        return { comments, pgCount, total };
    } catch (error) {
        console.error(error);
    }
};

export const createComment = async (commentData) => {
    try {
        const { data } = await axiosInstance.post(`api/comments`, {
            data: {
                ...commentData,
            },
        });
        return data;
    } catch (error) {
        console.error(error);
    }
};

export const addCommentReport = async (commentId, user) => {
    try {
        const { data } = await axiosInstance.post(`api/comments-reports`, {
            data: {
                comment: commentId,
                reporter: user,
                moderator: null,
            },
        });
        return data;
    } catch (error) {
        console.error(error);
    }
};
export const removeCommentReport = async (commentReportId) => {
    try {
        const { data } = await axiosInstance.delete(
            `api/comments-reports/${commentReportId}`
        );
        return data;
    } catch (error) {
        console.error(error);
    }
};
export const getCommentReports = async (query) => {
    try {
        const { data } = await axiosInstance.get(`api/comments/${query}`);
        return data;
    } catch (error) {
        console.error(error);
    }
};

export const approveCommentReport = async (comment, user) => {
    try {
        const { data } = await axiosInstance.put(`api/comments-reports/`);
        return data;
    } catch (error) {
        console.error(error);
    }
};
export const removeComment = async (comment, user) => {
    try {
        const { data } = await axiosInstance.put(`api/comments-reports/`);
        return data;
    } catch (error) {
        console.error(error);
    }
};
export const getCommentReportByHash = async (hash) => {
    try {
        const { data } = await axiosInstance.get(`api/comments-reports/`);
        return data;
    } catch (error) {
        console.error(error);
    }
};

export const createProposalLikeOrDislike = async ({ createData }) => {
    try {
        const { data } = await axiosInstance.post(`api/proposal-votes`, {
            data: {
                ...createData,
            },
        });
        return data?.data;
    } catch (error) {
        throw error;
    }
};
export const updateProposalLikesOrDislikes = async ({
    proposalVoteID,
    updateData,
}) => {
    try {
        const { data } = await axiosInstance.put(
            `api/proposal-votes/${proposalVoteID}`,
            {
                data: {
                    ...updateData,
                },
            }
        );
        return data?.data;
    } catch (error) {
        throw error;
    }
};

export const getUserProposalVote = async ({ proposalID }) => {
    try {
        const { data } = await axiosInstance.get(
            `/api/proposal-votes?filters[proposal_id][$eq]=${proposalID}`
        );

        return data.data;
    } catch (error) {
        return error;
    }
};

export const getUserPollVote = async ({ pollID }) => {
    try {
        const { data } = await axiosInstance.get(
            `/api/poll-votes?filters[poll_id][$eq]=${pollID}&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`
        );

        if (data?.data && data?.data?.length > 0) {
            return data.data[0];
        } else {
            return null;
        }
    } catch (error) {
        throw error;
    }
};

export const createPollVote = async ({ createData }) => {
    try {
        const { data } = await axiosInstance.post(`api/poll-votes`, {
            data: {
                ...createData,
            },
        });
        return data?.data;
    } catch (error) {
        throw error;
    }
};

export const getLoggedInUserInfo = async () => {
    try {
        const { data } = await axiosInstance.get(`api/users/me`, {
            withCredentials: true,
        });
        return data;
    } catch (error) {
        console.error(error);
    }
};

export const updatePollVote = async ({ pollVoteID, updateData }) => {
    try {
        const { data } = await axiosInstance.put(
            `api/poll-votes/${pollVoteID}`,
            {
                data: {
                    ...updateData,
                },
            }
        );
        return data?.data;
    } catch (error) {
        throw error;
    }
};
export const closePoll = async ({ pollID }) => {
    try {
        const { data } = await axiosInstance.put(`api/polls/${pollID}`, {
            data: {
                is_poll_active: false,
            },
        });
        return data?.data;
    } catch (error) {
        throw error;
    }
};
export const updateUser = async (updateData) => {
    try {
        const { data } = await axiosInstance.put(`/api/users/edit`, {
            ...updateData,
        });
        return data;
    } catch (error) {
        console.error(error);
        throw error?.response?.data?.error;
    }
};

export const getRefreshToken = async () => {
    try {
        const data = await axiosWithCookies.post(`/api/token/refresh`, {});
        return data.data;
    } catch (error) {
        throw error;
    }
};

export const getChallenge = async ({ query = '' }) => {
    try {
        const data = await axiosInstance.get(`/api/auth/challenge${query}`, {
            withCredentials: true,
        });
        return data.data;
    } catch (error) {
        throw error;
    }
};

export const getHardForkData = async () => {
    try {
        const data = await axiosInstance.get(
            '/api/proxy/govtool/proposal/enacted-details?type=HardForkInitiation'
        );
        return data.data;
    } catch (error) {
        throw error;
    }
};

export const getViaProxy = async (endpoint = '', payload) => {
    try {
        const { data } = await axiosInstance.post(`/api/proxy`, payload);
        return data;
    } catch (error) {
        console.error('Error posting data via govtool proxy:', error);
        throw error;
    }
};
