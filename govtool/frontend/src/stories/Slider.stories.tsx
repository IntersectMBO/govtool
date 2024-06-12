import type { Meta, StoryObj } from "@storybook/react";

import { Slider } from "@organisms";
import { expect } from "@storybook/jest";
import { within } from "@storybook/testing-library";

const DATA = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const BOX_SIZE = 200;

const meta = {
  title: "Example/Slider",
  component: Slider,
  args: {
    navigateKey: "key",
    title: "Slider title",
    isShowAll: false,
    data: DATA.slice(0, 6).map((item) => (
      <div
        className="keen-slider__slide"
        data-testid="slider"
        style={{
          alignItems: "center",
          backgroundColor: "aqua",
          height: BOX_SIZE,
          justifyItems: "center",
          minWidth: BOX_SIZE,
          overflow: "visible",
        }}
      >
        {item}
      </div>
    )),
    dataLength: DATA.slice(0, 6).length,
    notSlicedDataLength: 6,
  },
  tags: ["autodocs"],
} satisfies Meta<typeof Slider>;

export default meta;
type Story = StoryObj<typeof meta>;

export const SliderComponent: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText("Slider title")).toBeInTheDocument();
    await expect(canvas.getAllByTestId("slider")).toHaveLength(6);
  },
};

export const SliderComponentOverflow: Story = {
  args: {
    isShowAll: true,
    notSlicedDataLength: DATA.length,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText("Slider title")).toBeInTheDocument();
    await expect(canvas.getAllByTestId("slider")).toHaveLength(6);

    await expect(canvas.getByRole("button")).toBeEnabled();
  },
};
