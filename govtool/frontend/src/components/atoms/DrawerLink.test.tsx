import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { MemoryRouter, Route, Routes } from "react-router-dom";
import { DrawerLink } from "@atoms";
import { theme } from "@/theme";

describe("DrawerLink", () => {
  const mockOnClick = vi.fn();

  it("renders correctly with mandatory props", () => {
    render(
      <MemoryRouter initialEntries={["/somepath"]}>
        <DrawerLink label="Home" navTo="/home" />
      </MemoryRouter>,
    );

    const linkElement = screen.getByRole("link");
    expect(linkElement).toHaveAttribute("href", "/home");
    expect(screen.getByText("Home")).toBeInTheDocument();
  });

  it("applies active styles correctly when active", () => {
    render(
      <MemoryRouter initialEntries={["/home"]}>
        <Routes>
          <Route
            path="/home"
            element={<DrawerLink label="Home" navTo="/home" />}
          />
        </Routes>
      </MemoryRouter>,
    );

    const linkElement = screen.getByRole("link");
    expect(linkElement).toHaveStyle(
      `backgroundColor: ${theme.palette.highlightBlue}`,
    );
  });

  it("does not apply active styles when not active", () => {
    render(
      <MemoryRouter initialEntries={["/other"]}>
        <DrawerLink label="Home" navTo="/home" />
      </MemoryRouter>,
    );

    const linkElement = screen.getByRole("link");
    expect(linkElement).not.toHaveStyle(
      `backgroundColor: ${theme.palette.highlightBlue}`,
    );
  });

  it("renders with an icon and activeIcon", () => {
    const icon = "icon-path.png";
    const activeIcon = "active-icon-path.png";

    render(
      <MemoryRouter initialEntries={["/home"]}>
        <Routes>
          <Route
            path="/home"
            element={
              <DrawerLink
                label="Home"
                navTo="/home"
                icon={icon}
                activeIcon={activeIcon}
              />
            }
          />
        </Routes>
      </MemoryRouter>,
    );

    const img = screen.getByAltText("icon") as HTMLImageElement;
    expect(img.src).toContain("active-icon-path.png");
  });

  it("executes onClick callback when clicked", () => {
    render(
      <MemoryRouter initialEntries={["/home"]}>
        <DrawerLink label="Clickable" navTo="/click" onClick={mockOnClick} />
      </MemoryRouter>,
    );

    const linkElement = screen.getByRole("link");
    fireEvent.click(linkElement);
    expect(mockOnClick).toHaveBeenCalled();
  });
});
