import { en } from "@/translations/locales/en";

declare module "i18next" {
  interface CustomTypeOptions {
    defaultNS: "en";

    resources: {
      en: (typeof en)["translation"];
    };
  }
}
