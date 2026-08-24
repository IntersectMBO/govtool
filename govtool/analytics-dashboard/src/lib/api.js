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

export default getGoogleData;const MOCKAROO_API_KEY = process.env.MOCKAROO_API_KEY || process.env.NEXT_PUBLIC_MOCKAROO_API_KEY;
if (!MOCKAROO_API_KEY) {
  throw new Error("Mockaroo API key is not defined. Please set MOCKAROO_API_KEY in your environment variables.");
}
const url = `https://my.api.mockaroo.com/proposals.json?key=${MOCKAROO_API_KEY}`;
