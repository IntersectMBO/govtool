import axios from 'axios';
import { getDataFromSession } from './utils';

let axiosGovTools = axios.create();
const BASE_URL = import.meta.env.VITE_BASE_URL;
export const setAxiosBaseURL = (baseURL) => {
    axiosInstance.defaults.baseURL =
        baseURL || BASE_URL;
};

axiosGovTools.interceptors.request.use((config) => {
    const token =
        typeof window !== 'undefined' && getDataFromSession('pdfUserJwt');
    if (token) {
        config.headers['Authorization'] = `Bearer ${token}`;
    }
    return config;
});

export default axiosGovTools;
