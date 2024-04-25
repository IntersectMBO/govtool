// Import createMiddleware from next-intl to configure internationalization middleware for Next.js.
import createMiddleware from "next-intl/middleware";
import { defaultLocale, locales } from "./constants";

// Export the middleware configuration to define supported locales and the default locale.
// This setup applies internationalization strategies across the application.
export default createMiddleware({
	locales: locales, // Specify the supported locales for the application.
	defaultLocale: defaultLocale, // Set the default locale to be used when no other locale matches.
});

// Define and export a config object to specify which paths the middleware should apply to.
// This ensures the internationalization logic only runs for specified routes.
export const config = {
	matcher: ["/", "/(de|en)/:path*"], // Apply middleware to the root path and any path prefixed with supported locales.
};
