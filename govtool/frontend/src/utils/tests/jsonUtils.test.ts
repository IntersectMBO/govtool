import { vi } from "vitest";
import { downloadJson } from "..";

describe("downloadJson", () => {
  it("downloads JSON with default file name", () => {
    const json = { name: "John Doe", age: 30 };
    const linkMock = document.createElement("a");
    const clickMock = vi.fn();
    linkMock.click = clickMock;

    vi.spyOn(document, "createElement").mockReturnValue(linkMock);

    downloadJson(json);

    expect(linkMock.href).toBe(
      "data:text/jsonld;charset=utf-8,%7B%0A%20%20%22name%22%3A%20%22John%20Doe%22%2C%0A%20%20%22age%22%3A%2030%0A%7D",
    );
    expect(linkMock.download).toBe("data.jsonld");
    expect(clickMock).toHaveBeenCalled();

    vi.restoreAllMocks();
  });

  it("downloads JSON with custom file name", () => {
    const json = { name: "John Doe", age: 30 };
    const linkMock = document.createElement("a");
    const clickMock = vi.fn();
    linkMock.click = clickMock;

    vi.spyOn(document, "createElement").mockReturnValue(linkMock);

    downloadJson(json, "custom");

    expect(linkMock.href).toBe(
      "data:text/jsonld;charset=utf-8,%7B%0A%20%20%22name%22%3A%20%22John%20Doe%22%2C%0A%20%20%22age%22%3A%2030%0A%7D",
    );
    expect(linkMock.download).toBe("custom.jsonld");
    expect(clickMock).toHaveBeenCalled();

    vi.restoreAllMocks();
  });
});
