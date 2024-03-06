import i18n from "@/i18n";
import { ICONS } from "./icons";
import { PATHS } from "./paths";

export const NAV_ITEMS = [
  {
    dataTestId: "dashboard-link",
    navTo: PATHS.home,
    label: "Dashboard",
    newTabLink: null,
  },
  {
    dataTestId: "drep-directory-link",
    navTo: PATHS.dRepDirectory,
    label: i18n.t("dRepDirectory.title"),
  },
  {
    dataTestId: "governance-actions-link",
    navTo: PATHS.governanceActions,
    label: "Governance Actions",
    newTabLink: null,
  },
  {
    dataTestId: "guides-link",
    navTo: "",
    label: "Guides",
    newTabLink: "https://docs.sanchogov.tools/about/what-is-sanchonet-govtool",
  },
  {
    dataTestId: "faqs-link",
    navTo: "",
    label: "FAQs",
    newTabLink: "https://docs.sanchogov.tools/faqs",
  },
];

export const CONNECTED_NAV_ITEMS = [
  {
    dataTestId: "dashboard-link",
    label: "Dashboard",
    navTo: PATHS.dashboard,
    activeIcon: ICONS.dashboardActiveIcon,
    icon: ICONS.dashboardIcon,
    newTabLink: null,
  },
  {
    dataTestId: "drep-directory-link",
    label: i18n.t("dRepDirectory.title"),
    navTo: PATHS.dashboardDRepDirectory,
    activeIcon: ICONS.dRepDirectoryActiveIcon,
    icon: ICONS.dRepDirectoryIcon,
  },
  {
    dataTestId: "governance-actions-link",
    label: "Governance Actions",
    navTo: PATHS.dashboardGovernanceActions,
    activeIcon: ICONS.governanceActionsActiveIcon,
    icon: ICONS.governanceActionsIcon,
    newTabLink: null,
  },
  {
    dataTestId: "guides-link",
    label: "Guides",
    navTo: "",
    activeIcon: ICONS.guidesActiveIcon,
    icon: ICONS.guidesIcon,
    newTabLink: "https://docs.sanchogov.tools/about/what-is-sanchonet-govtool",
  },
  {
    dataTestId: "faqs-link",
    label: "FAQs",
    navTo: "",
    activeIcon: ICONS.faqsActiveIcon,
    icon: ICONS.faqsIcon,
    newTabLink: "https://docs.sanchogov.tools/faqs",
  },
];
