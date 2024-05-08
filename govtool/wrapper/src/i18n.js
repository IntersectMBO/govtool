// Import getRequestConfig from next-intl for configuring request-level internationalization.
import { getRequestConfig } from "next-intl/server";
import { notFound } from "next/navigation";
import { locales } from "./constants";

// This setup function is called for each request to determine the appropriate locale messages to load.
export default getRequestConfig(async ({ locale }) => {
	// Check if the requested locale is supported. If not, trigger a notFound response.
	if (!locales.includes(locale)) notFound();

	// Dynamically import the JSON messages file for the requested locale and return it.
	// This allows serving locale-specific content without hardcoding the locale data.
	return {
		messages: (await import(`../messages/${locale}.json`)).default,
	};
});
