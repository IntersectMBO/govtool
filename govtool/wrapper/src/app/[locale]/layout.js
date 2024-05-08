import { locales } from "@/constants";
import { AppContextProvider } from "@/context/context";
import { NextIntlClientProvider } from "next-intl";
import { unstable_setRequestLocale } from "next-intl/server";
import { Inter } from "next/font/google";
import { notFound } from "next/navigation";
const inter = Inter({ subsets: ["latin"] });

export function generateStaticParams() {
	// Generate static params for each locale, used in static generation methods.
	return locales.map((locale) => ({ locale }));
}

// Define common metadata for the application.
export const metadata = {
	title: "Web App Boilerplate",
	description: "Web App Boilerplate",
};

async function RootLayout({ children, params: { locale } }) {
	// Root layout component, sets up locale, loads messages, and wraps the app with providers.
	unstable_setRequestLocale(locale); // Set the locale for the request, use with caution due to unstable API.
	if (!locales.includes(locale)) notFound(); // Check if the locale is supported, otherwise trigger a 404.

	let messages;
	try {
		// Attempt to dynamically load the message bundle for the current locale.
		messages = (await import(`../../../messages/${locale}.json`)).default;
	} catch (error) {
		notFound(); // Trigger a 404 if the message bundle cannot be loaded.
	}

	return (
		// Set the document language
		<html lang={locale}>
			<head>
				<title>{metadata.title}</title>
				<link rel="icon" href="/favicon.ico" sizes="any" />
			</head>
			{/* Apply font class and suppress hydration warning. */}
			<body className={inter.className} suppressHydrationWarning={true}>
				{/* Provide internationalization context. */}
				<NextIntlClientProvider locale={locale} messages={messages}>
					{/* Wrap children in global state context */}
					<AppContextProvider>{children}</AppContextProvider>
				</NextIntlClientProvider>
			</body>
		</html>
	);
}

export default RootLayout;
