import axiosGovTools from "./axiosgovtools";

export const getDrepInfo = async (drepHash) => {
    try {
        const { data } = await axiosGovTools.get(`/api/drep-info/${drepHash}`);
        return data;
    } catch (error) {
        throw error;
    }
}