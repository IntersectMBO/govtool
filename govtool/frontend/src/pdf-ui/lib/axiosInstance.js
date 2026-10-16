import axios from 'axios';
import { getDataFromSession } from './utils';

let axiosInstance = axios.create();
let axiosWithCookies = axios.create();

export const setAxiosBaseURL = (baseURL) => {
    axiosInstance.defaults.baseURL =
        baseURL || process.env.NEXT_PUBLIC_PROPOSAL_DISCUSSION_API_URL;
    axiosWithCookies.defaults.baseURL =
        baseURL || process.env.NEXT_PUBLIC_PROPOSAL_DISCUSSION_API_URL;
};

axiosInstance.interceptors.request.use((config) => {
    const token =
        typeof window !== 'undefined' && getDataFromSession('pdfUserJwt');
    if (token) {
        config.headers['Authorization'] = `Bearer ${token}`;
    }
    return config;
});

axiosWithCookies.defaults.withCredentials = true;

export default axiosInstance;
export { axiosWithCookies };


