import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, within } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";
import type { GovToolNotification } from "@/models";
import { DEFAULT_PREFERENCES } from "@/utils/notifications";

import { NotificationBell } from "./NotificationBell";
import { NotificationCenter } from "./NotificationCenter";
import { NotificationPreferencesPanel } from "./NotificationPreferencesPanel";

const mocks = vi.hoisted(() => ({
  navigate: vi.fn(),
  args: [] as Record<string, unknown>[],
  feed: {} as Record<string, unknown>,
  isMobile: false,
}));

vi.mock("react-router", async (importOriginal) => ({
  ...(await importOriginal<typeof import("react-router")>()),
  useNavigate: () => mocks.navigate,
}));
vi.mock("@/hooks/useScreenDimension", () => ({
  useScreenDimension: () => ({ isMobile: mocks.isMobile }),
}));
vi.mock("@/hooks/useNotifications", () => ({
  NOTIFICATION_PAGE_SIZE: 7,
  useNotifications: (args: Record<string, unknown> = {}) => {
    mocks.args.push(args);
    if (mocks.feed.throws) throw new Error("bad service data");
    return mocks.feed;
  },
}));

const NOW = Date.now();
const item = (n: number, over: Partial<GovToolNotification> = {}) =>
  ({
    id: `n${n}`,
    kind: "delegationChange",
    createdAt: new Date(NOW - n * 60 * 60 * 1000).toISOString(),
    read: false,
    data: {
      change: "delegated",
      target: { type: "drep", dRepId: `drep1x${n}`, dRepName: `DRep ${n}` },
      votingPower: null,
    },
    ...over,
  }) as GovToolNotification;

const feed = (over: Record<string, unknown> = {}) => ({
  mode: "local",
  needsSignIn: false,
  signIn: vi.fn(),
  items: [item(1), item(2, { read: true })],
  total: 2,
  counts: { all: 2, unread: 1, governanceActions: 0, delegation: 2, drep: 0, myActivity: 2 },
  unread: 1,
  hasMore: false,
  loadMore: vi.fn(),
  isLoading: false,
  isError: false,
  retry: vi.fn(),
  markRead: vi.fn(),
  markAllRead: vi.fn(),
  preferences: DEFAULT_PREFERENCES,
  savePreferences: vi.fn(),
  capabilities: { email: false, crossDevice: false, events: ["drepChange", "delegationChange", "actionActivity", "relevantAction", "digest"] },
  ...over,
});

const renderIn = (node: React.ReactNode) => render(<MemoryRouter>{node}</MemoryRouter>);

describe("NotificationBell", () => {
  beforeEach(() => {
    mocks.navigate.mockReset();
    mocks.isMobile = false;
    mocks.feed = feed();
  });

  it("says how many are unread and lists the latest in a popover", () => {
    renderIn(<NotificationBell />);
    const bell = screen.getByRole("button", { name: "Notifications, 1 unread" });
    fireEvent.click(bell);
    expect(bell).toHaveAttribute("aria-expanded", "true");
    const popover = screen.getByTestId("notifications-popover");
    expect(within(popover).getAllByTestId("notifications-popover-item")).toHaveLength(2);
    fireEvent.click(within(popover).getByTestId("notifications-mark-all-read"));
    expect((mocks.feed.markAllRead as ReturnType<typeof vi.fn>)).toHaveBeenCalled();
    fireEvent.click(within(popover).getByTestId("notifications-view-all"));
    expect(mocks.navigate).toHaveBeenCalledWith("/notifications");
  });

  it("renders nothing rather than breaking the header when its data is unusable", () => {
    const error = vi.spyOn(console, "error").mockImplementation(() => {});
    mocks.feed = { ...feed(), throws: true };
    const { container } = renderIn(<NotificationBell />);
    expect(container).toBeEmptyDOMElement();
    error.mockRestore();
  });

  it("goes straight to the Notification Center on a phone", () => {
    mocks.isMobile = true;
    renderIn(<NotificationBell />);
    fireEvent.click(screen.getByTestId("notifications-bell"));
    expect(mocks.navigate).toHaveBeenCalledWith("/notifications");
    expect(screen.queryByTestId("notifications-popover")).toBeNull();
  });
});

describe("NotificationCenter", () => {
  beforeEach(() => {
    mocks.args = [];
    mocks.feed = feed();
  });

  it("groups the feed by day and says it is worked out in this browser", () => {
    renderIn(<NotificationCenter />);
    expect(screen.getByRole("heading", { level: 1 })).toHaveTextContent("Notification Center");
    expect(screen.getByTestId("notifications-local-note")).toBeInTheDocument();
    expect(screen.getAllByTestId("notification-card")).toHaveLength(2);
    expect(screen.getByTestId("notifications-showing")).toHaveTextContent("Showing 1–2 of 2 notifications");
  });

  it("filters by type and marks one item read or unread", () => {
    renderIn(<NotificationCenter />);
    fireEvent.click(screen.getByTestId("notifications-filter-delegation"));
    expect(mocks.args.at(-1)).toMatchObject({ filter: "delegation" });
    const [first, second] = screen.getAllByTestId("notification-toggle-read");
    fireEvent.click(first);
    fireEvent.click(second);
    expect(mocks.feed.markRead).toHaveBeenCalledWith(["n1"], true);
    expect(mocks.feed.markRead).toHaveBeenCalledWith(["n2"], false);
  });

  it("shows the empty, error and sign-in states", () => {
    mocks.feed = feed({ items: [], total: 0, unread: 0 });
    const { unmount } = renderIn(<NotificationCenter />);
    expect(screen.getByTestId("notifications-empty")).toHaveTextContent("No notifications yet");
    unmount();

    mocks.feed = feed({ isError: true, items: [] });
    const second = renderIn(<NotificationCenter />);
    fireEvent.click(screen.getByText("Retry feed"));
    expect(mocks.feed.retry).toHaveBeenCalled();
    second.unmount();

    mocks.feed = feed({ mode: "remote", needsSignIn: true, items: [] });
    renderIn(<NotificationCenter />);
    expect(screen.queryByTestId("notifications-local-note")).toBeNull();
    fireEvent.click(screen.getByTestId("notifications-sign-in"));
    expect(mocks.feed.signIn).toHaveBeenCalled();
  });

  it("loads more", () => {
    mocks.feed = feed({ hasMore: true, total: 12 });
    renderIn(<NotificationCenter />);
    fireEvent.click(screen.getByTestId("notifications-load-more"));
    expect(mocks.feed.loadMore).toHaveBeenCalled();
  });
});

describe("NotificationPreferencesPanel", () => {
  beforeEach(() => {
    mocks.feed = feed();
  });

  it("keeps email off until the notification service offers it", () => {
    renderIn(<NotificationPreferencesPanel />);
    const email = screen.getByTestId("notification-channel-email");
    expect(email).toBeDisabled();
    expect(email).not.toBeChecked();
  });

  it("saves a switched-off event and confirms it", async () => {
    renderIn(<NotificationPreferencesPanel />);
    fireEvent.click(screen.getByTestId("notification-event-digest"));
    fireEvent.click(screen.getByTestId("notification-preferences-save"));
    expect(mocks.feed.savePreferences).toHaveBeenCalledWith({
      ...DEFAULT_PREFERENCES,
      events: { ...DEFAULT_PREFERENCES.events, digest: "off" },
    });
    expect(await screen.findByTestId("notification-preferences-status")).toHaveTextContent(
      "Preferences saved",
    );
  });

  it("offers email once the service supports it, with the address or a setup link", () => {
    mocks.feed = feed({
      mode: "remote",
      capabilities: { email: true, emailSetupUrl: "https://gov.tools/email", crossDevice: true, events: ["drepChange"] },
    });
    const { unmount } = renderIn(<NotificationPreferencesPanel />);
    expect(screen.getByTestId("notification-channel-email")).toBeEnabled();
    expect(screen.queryByTestId("notification-event-digest")).toBeNull();
    expect(screen.getByRole("link", { name: /Add an email address/ })).toHaveAttribute(
      "href",
      "https://gov.tools/email",
    );
    unmount();
    mocks.feed = feed({
      mode: "remote",
      preferences: {
        ...DEFAULT_PREFERENCES,
        channels: { inApp: true, email: { enabled: true, address: "ada@mail.com", status: "verified" } },
      },
      capabilities: { email: true, emailSetupUrl: null, crossDevice: true, events: ["drepChange"] },
    });
    renderIn(<NotificationPreferencesPanel />);
    expect(screen.getByText("Send to ada@mail.com.")).toBeInTheDocument();
  });

  it("describes each switch for screen readers", () => {
    renderIn(<NotificationPreferencesPanel />);
    expect(screen.getByTestId("notification-channel-email")).toHaveAccessibleDescription(
      /Not available yet/,
    );
  });

  it("asks to sign in before showing or saving the service's preferences", () => {
    mocks.feed = feed({ mode: "remote", needsSignIn: true });
    renderIn(<NotificationPreferencesPanel />);
    expect(screen.getByTestId("notifications-sign-in-panel")).toBeInTheDocument();
    expect(screen.queryByTestId("notification-preferences-save")).toBeNull();
  });
});
