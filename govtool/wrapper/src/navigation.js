// Importing createSharedPathnamesNavigation from next-intl/navigation to initialize locale-aware navigation helpers.
import { createSharedPathnamesNavigation } from "next-intl/navigation";
import { locales } from "./constants";

// Setting the locale prefixing strategy to 'always', ensuring URLs always include the locale.
export const localePrefix = "always"; // Default

// Destructuring and exporting the navigation helpers from createSharedPathnamesNavigation.
// This creates and configures custom Link, redirect, usePathname, and useRouter functions that are aware of the defined locales and prefixing strategy.
// These helpers should be used throughout the application to ensure navigation is handled correctly with respect to the active locale.
export const { Link, redirect, usePathname, useRouter } =
	createSharedPathnamesNavigation({ locales: locales, localePrefix });
