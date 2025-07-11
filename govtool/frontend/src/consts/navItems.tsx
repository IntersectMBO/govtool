import { IconAcademicCap } from "@intersect.mbo/intersectmbo.org-icons-set";
import i18n from "@/i18n";
import { theme } from "@/theme";

import { ICONS } from "./icons";
import { LINKS } from "./links";
import {
  PATHS,
  PDF_PATHS,
  OUTCOMES_PATHS,
  BUDGET_DISCUSSION_PATHS,
  // TODO: This will be uncommented when the page has been bootstrapped in the outcomes Pillar
  // USER_PATHS
} from "./paths";

export type NavItem = {
  dataTestId: string;
  navTo: string;
  label: string;
  newTabLink: string | null;
};
export type NavMenuItem = {
  dataTestId: string;
  label: string;
  childNavItems?: Array<NavItem>;
};
export const NAV_ITEMS: Array<NavItem | NavMenuItem> = [
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
    newTabLink: null,
  },
  {
    dataTestId: "budget-discussion-link",
    navTo: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
    label: i18n.t("budgetDiscussion.title"),
    newTabLink: null,
  },
  {
    dataTestId: "governance-actions",
    label: i18n.t("govActions.categoryTitle"),
    childNavItems: [
      {
        dataTestId: "proposed-governance-actions-link",
        navTo: PDF_PATHS.proposalDiscussion,
        label: i18n.t("proposalDiscussion.title"),
        newTabLink: null,
      },
      {
        dataTestId: "governance-actions-link",
        navTo: PATHS.governanceActions,
        label: i18n.t("govActions.title"),
        newTabLink: null,
      },
      {
        dataTestId: "governance-actions-outcomes-link",
        label: i18n.t("govActions.outcomes.title"),
        navTo: OUTCOMES_PATHS.governanceActionsOutcomes,
        newTabLink: null,
      },
    ],
  },
  {
    dataTestId: "guides-link",
    navTo: "",
    label: i18n.t("menu.guides"),
    newTabLink: LINKS.USING_GOVTOOL,
  },
  {
    dataTestId: "faqs-link",
    navTo: "",
    label: i18n.t("menu.faqs"),
    newTabLink: LINKS.FAQS,
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
    newTabLink: null,
  },
  {
    dataTestId: "budget-discussion-link",
    label: i18n.t("budgetDiscussion.title"),
    navTo: BUDGET_DISCUSSION_PATHS.budgetDiscussion,
    activeIcon: ICONS.dRepDirectoryActiveIcon,
    icon: ICONS.dRepDirectoryIcon,
    newTabLink: null,
  },
  {
    dataTestId: "governance-actions-link",
    label: i18n.t("govActions.categoryTitle"),
    navTo: PATHS.dashboardGovernanceActions,
    activeIcon: ICONS.governanceActionsActiveIcon,
    icon: ICONS.governanceActionsIcon,
    newTabLink: null,
    childNavItems: [
      {
        dataTestId: "proposal-discussion-link",
        label: i18n.t("proposalDiscussion.title"),
        navTo: PDF_PATHS.proposalDiscussion,
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
        dataTestId: "governance-actions-live-voting-link",
        label: i18n.t("govActions.liveVoting.title"),
        navTo: PATHS.dashboardGovernanceActions,
        activeIcon: ICONS.governanceActionsActiveIcon,
        icon: ICONS.governanceActionsIcon,
        newTabLink: null,
      },
      {
        dataTestId: "governance-actions-outcomes-link",
        label: i18n.t("govActions.outcomes.title"),
        navTo: OUTCOMES_PATHS.governanceActionsOutcomes,
        activeIcon: ICONS.governanceActionsActiveIcon,
        icon: ICONS.governanceActionsIcon,
        newTabLink: null,
      },
      // TODO: This will be uncommented when the page has been bootstrapped in the outcomes Pillar
      // {
      //   dataTestId: "governance-actions-voted-by-me-link",
      //   label: i18n.t("govActions.votedByMe.title"),
      //   navTo: USER_PATHS.governanceActionsVotedByMe,
      //   activeIcon: ICONS.governanceActionsActiveIcon,
      //   icon: ICONS.governanceActionsIcon,
      //   newTabLink: null,
      // },
    ],
  },
  {
    dataTestId: "guides-link",
    label: i18n.t("menu.guides"),
    navTo: "",
    activeIcon: ICONS.guidesActiveIcon,
    icon: ICONS.guidesIcon,
    newTabLink: LINKS.USING_GOVTOOL,
  },
  {
    dataTestId: "faqs-link",
    label: i18n.t("menu.faqs"),
    navTo: "",
    activeIcon: ICONS.faqsActiveIcon,
    icon: ICONS.faqsIcon,
    newTabLink: LINKS.FAQS,
  },
];
