import type { Meta, StoryObj } from "@storybook/react";

import { Slider } from "@organisms";
import { within } from "@storybook/testing-library";
import { expect } from "@storybook/jest";

const DATA = [1, 2, 3, 4, 5, 6];
const BOX_SIZE = 200;

const meta = {
  title: "Example/Slider",
  component: Slider,
  tags: ["autodocs"],
} satisfies Meta<typeof Slider>;

export default meta;
type Story = StoryObj<typeof meta>;

export const SliderComponent: Story = {
  args: {
    navigateKey: "key",
    title: "Slider title",
    data: DATA.map((item) => (
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
    dataLength: DATA.length,
  },
  play: ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText("Slider title")).toBeInTheDocument();
    expect(canvas.getAllByTestId("slider")).toHaveLength(DATA.length);
  },
};
