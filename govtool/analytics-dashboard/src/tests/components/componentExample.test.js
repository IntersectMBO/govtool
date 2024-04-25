import React from "react";
import { render, screen } from "@testing-library/react";
import ComponentExample from "@/components/ComponentExample";

test("renders the sum of two numbers", () => {
  render(<ComponentExample a={2} b={3} />);
  const sumElement = screen.getByText("5");
  expect(sumElement).toBeInTheDocument();
});
