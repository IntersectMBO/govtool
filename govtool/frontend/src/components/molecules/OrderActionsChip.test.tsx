import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";

// `src/consts` and `src/theme` import each other (`navItems` reads the palette
// at module scope). The cycle only resolves when the consts barrel is the one
// that starts evaluating, so it is pulled in first here, before the component.
import "@consts";

import { OrderActionsChip } from "./OrderActionsChip";

// `OrderActionsChip` only needs a label function and the mobile breakpoint; the
// real hooks pull in i18n initialisation and a window resize listener that this
// test has no use for.
vi.mock("@hooks", () => ({
  useTranslation: () => ({ t: (key: string) => key }),
  useScreenDimension: () => ({ isMobile: false }),
}));

const renderChip = (isSorting?: boolean) =>
  render(
    <OrderActionsChip
      chosenSorting=""
      sortOpen={false}
      setSortOpen={() => {}}
      isFiltering={false}
      isSorting={isSorting}
    />,
  );

describe("OrderActionsChip", () => {
  it("renders the sort chip by default", () => {
    renderChip();

    expect(screen.getByTestId("sort-button")).toBeInTheDocument();
  });

  it("hides the sort chip when nothing can be sorted", () => {
    // The contract's rule for an empty allow-list: hide the control, never
    // render a dropdown with no options in it.
    renderChip(false);

    expect(screen.queryByTestId("sort-button")).not.toBeInTheDocument();
  });
});
