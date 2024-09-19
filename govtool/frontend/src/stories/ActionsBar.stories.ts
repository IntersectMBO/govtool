import {
  GOVERNANCE_ACTIONS_FILTERS,
  GOVERNANCE_ACTIONS_SORTING,
} from "@consts";
import { expect, jest } from "@storybook/jest";
import type { Meta, StoryObj } from "@storybook/react";
import { userEvent, within } from "@storybook/testing-library";
import { DataActionsBar } from "@/components/molecules";

const meta = {
  title: "Example/DataActionsBar",
  component: DataActionsBar,
  args: {
    filterOptions: GOVERNANCE_ACTIONS_FILTERS,
    sortOptions: GOVERNANCE_ACTIONS_SORTING,
  },
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof DataActionsBar>;

export default meta;
type Story = StoryObj<typeof meta>;

export const ActionsBarComponent: Story = {
  args: {
    chosenSorting: "",
    searchText: "",
    sortOpen: false,
    filterOptions: GOVERNANCE_ACTIONS_FILTERS,
    isFiltering: true,
    setFiltersOpen: jest.fn(),
    setSortOpen: jest.fn(),
    setSearchText: jest.fn(),
  },
  play: async ({ args, canvasElement, step }) => {
    const { setFiltersOpen, setSortOpen, setSearchText } = args;
    const canvas = within(canvasElement);

    await step("Check if searching is working", async () => {
      const searchInput = canvas.getByTestId("search-input");
      await userEvent.type(searchInput, "info action");
      await expect(setSearchText).toHaveBeenCalled();
    });

    await step("Check if filter button is working", async () => {
      await userEvent.click(canvas.getByTestId("filters-button"));
      // Make sure sort dropdown is closed
      await expect(setSortOpen).toHaveBeenCalledWith(false);
      await expect(setFiltersOpen).toHaveBeenCalled();
    });

    await step("Check if sort button is working", async () => {
      await userEvent.click(canvas.getByTestId("sort-button"));
      // Make sure filter dropdown is closed
      await expect(setFiltersOpen).toHaveBeenCalledWith(false);
      await expect(setSortOpen).toHaveBeenCalled();
    });
  },
};

export const ActionsBarFiltersOpen: Story = {
  args: {
    chosenSorting: "",
    searchText: "",
    sortOpen: false,
    chosenFilters: [],
    closeFilters: () => {},
    filtersOpen: true,
    setFiltersOpen: () => {},
    setChosenFilters: jest.fn(),
  },

  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);
    GOVERNANCE_ACTIONS_FILTERS.forEach(async ({ key, label }) => {
      await userEvent.click(
        canvas.getByTestId(`${label.replace(/ /g, "")}-checkbox`),
      );
      expect(args.setChosenFilters).toHaveBeenCalledWith([key]);
    });
  },
};

export const ActionsBarSortsOpen: Story = {
  args: {
    chosenSorting: "",
    searchText: "",
    sortOpen: true,
    setChosenSorting: jest.fn(),
  },
  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);
    GOVERNANCE_ACTIONS_SORTING.forEach(async ({ key }) => {
      await userEvent.click(canvas.getByTestId(`${key}-radio`));
      expect(args.setChosenSorting).toHaveBeenCalledWith(key);
    });
  },
};

export const ActionsBarWithoutFilters: Story = {
  args: {
    chosenSorting: "",
    searchText: "",
    sortOpen: false,
    isFiltering: false,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.queryByTestId("filters-button"),
    ).not.toBeInTheDocument();
  },
};
