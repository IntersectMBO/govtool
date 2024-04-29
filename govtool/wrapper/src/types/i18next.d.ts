import { en } from "@/i18n/locales/en";

declare module "i18next" {
  interface CustomTypeOptions {
    defaultNS: "en";

    resources: {
      en: (typeof en)["translation"];
    };
  }
}
