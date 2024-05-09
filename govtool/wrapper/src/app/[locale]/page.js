import { useTranslations } from "next-intl"; // Import hook for loading translated strings.
import { unstable_setRequestLocale } from "next-intl/server";

import Link from "next/link";

export default function Home({ params: { locale } }) {
  unstable_setRequestLocale(locale); // Sets the locale for the request. Use cautiously due to its unstable nature.
  const t = useTranslations("Index"); // Use the useTranslations hook to load translations for the "Index" namespace.

  return (
    <main>
      <div>
        {/*Render the translated title. */}
        <h1>{t("title")}</h1>
        <Link href={`/${locale}/metadata`}>Go to metadata service</Link>
        <br />
        <Link href={`/${locale}/wallet`}>Go to wallet connector</Link>
      </div>
    </main>
  );
}
