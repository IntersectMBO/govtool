// Import the Axios library, which is used for making HTTP requests.
import axios from "axios";

// Define the base URL for the Axios instance. This uses an environment variable for flexibility,
// defaulting to "http://localhost:1337" if the environment variable is not set.
// This is useful for differentiating between development and production environments.
const baseURL = process.env.NEXT_PUBLIC_API_URL || "http://localhost:1337";

// Create a customized instance of Axios with the defined base URL.
// This instance will inherit all the default settings of Axios, but will use the specified baseURL
// for all the requests, making it unnecessary to repeatedly specify the baseURL in every request.
const axiosInstance = axios.create({
	baseURL,
});

// Export the customized Axios instance for use throughout the application.
// This allows for a consistent configuration and simplifies making API requests by
// pre-configuring the base part of the request URLs.
export default axiosInstance;
