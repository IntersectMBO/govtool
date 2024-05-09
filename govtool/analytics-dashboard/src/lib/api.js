import axiosInstance from "@/lib/axiosInstance";

//This file contains API functions to interact with the backend
async function getGoogleData() {
    try {
        const { data } = await axiosInstance.get('/api/analytics');
        return data;
    } catch (error) {
        return [];
    }
}

export default getGoogleData;