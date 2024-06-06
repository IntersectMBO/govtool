import { IconAcademicCap } from "@intersect.mbo/intersectmbo.org-icons-set";
import i18n from "@/i18n";
import { ICONS } from "./icons";
import { PATHS, PDF_PATHS } from "./paths";
import { theme } from "@/theme";

export const NAV_ITEMS = [
  {
    dataTestId: "dashboard-link",
    navTo: PATHS.home,
    label: i18n.t("dashboard.title"),
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
    label: i18n.t("govActions.title"),
    newTabLink: null,
  },
  {
    dataTestId: "guides-link",
    navTo: "",
    label: i18n.t("menu.guides"),
    newTabLink: "https://docs.sanchogov.tools/about/what-is-sanchonet-govtool",
  },
  {
    dataTestId: "faqs-link",
    navTo: "",
    label: i18n.t("menu.faqs"),
    newTabLink: "https://docs.sanchogov.tools/faqs",
  },
];

export const CONNECTED_NAV_ITEMS = [
  {
    dataTestId: "dashboard-link",
    label: i18n.t("dashboard.title"),
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
    label: i18n.t("govActions.title"),
    navTo: PATHS.dashboardGovernanceActions,
    activeIcon: ICONS.governanceActionsActiveIcon,
    icon: ICONS.governanceActionsIcon,
    newTabLink: null,
  },
  {
    dataTestId: "proposal-discussion-link",
    label: i18n.t("proposalDiscussion.title"),
    navTo: `${PATHS.connectedProposalPillar.replace("/*", "")}${
      PDF_PATHS.proposalDiscussion
    }`,
    activeIcon: (
      <IconAcademicCap
        width="20"
        height="20"
        viewBox="0 0 24 24"
        fill={theme.palette.accentOrange}
      />
    ),
    icon: (
      <IconAcademicCap
        width="20"
        height="20"
        viewBox="0 0 24 24"
        fill={theme.palette.lightOrange}
      />
    ),
    newTabLink: null,
  },
  {
    dataTestId: "guides-link",
    label: i18n.t("menu.guides"),
    navTo: "",
    activeIcon: ICONS.guidesActiveIcon,
    icon: ICONS.guidesIcon,
    newTabLink: "https://docs.sanchogov.tools/about/what-is-sanchonet-govtool",
  },
  {
    dataTestId: "faqs-link",
    label: i18n.t("menu.faqs"),
    navTo: "",
    activeIcon: ICONS.faqsActiveIcon,
    icon: ICONS.faqsIcon,
    newTabLink: "https://docs.sanchogov.tools/faqs",
  },
];
