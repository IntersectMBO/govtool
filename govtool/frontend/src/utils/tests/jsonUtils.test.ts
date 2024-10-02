import { vi } from "vitest";
import { downloadJson } from "..";

describe("downloadJson", () => {
  beforeEach(() => {
    global.URL.createObjectURL = vi.fn(() => "mocked-url");
    global.URL.revokeObjectURL = vi.fn();

    // We should pass Node as an argument based on typing.
    // But we are not testing this against Nodes.
    /* eslint-disable @typescript-eslint/ban-ts-comment */
    // @ts-expect-error
    vi.spyOn(document.body, "appendChild").mockImplementation(() => undefined);
    // @ts-expect-error
    vi.spyOn(document.body, "removeChild").mockImplementation(() => undefined);
  });

  afterEach(() => {
    // Restore the mocks after each test
    vi.restoreAllMocks();
  });

  it("downloads JSON with default file name", () => {
    const json = { name: "John Doe", age: 30 };
    const linkMock = document.createElement("a");
    const clickMock = vi.fn();
    linkMock.click = clickMock;

    vi.spyOn(document, "createElement").mockReturnValue(linkMock);

    downloadJson(json);

    expect(linkMock.href).toBe("http://localhost:3000/mocked-url");

    expect(linkMock.download).toBe("data.jsonld");
    expect(global.URL.createObjectURL).toHaveBeenCalled();
    expect(clickMock).toHaveBeenCalled();
  });

  it("downloads JSON with custom file name", () => {
    const json = { name: "John Doe", age: 30 };
    const linkMock = document.createElement("a");
    const clickMock = vi.fn();
    linkMock.click = clickMock;

    vi.spyOn(document, "createElement").mockReturnValue(linkMock);

    downloadJson(json, "custom");

    expect(linkMock.href).toBe("http://localhost:3000/mocked-url");
    expect(linkMock.download).toBe("custom.jsonld");
    expect(clickMock).toHaveBeenCalled();
  });
});
