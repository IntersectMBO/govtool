import { render, screen } from "@testing-library/react";
import { MetadataProvider, useMetadata } from "@/providers/MetadataProvider";

describe("MetadataProvider", () => {
  it("renders its children", () => {
    render(
      <MetadataProvider>
        <div>Child Component</div>
      </MetadataProvider>
    );

    const childComponent = screen.getByText("Child Component");
    expect(childComponent).toBeDefined();
  });

  it("provides the validate and build functions in the context", () => {
    const TestComponent = () => {
      const { validate, build } = useMetadata();
      expect(typeof validate).toBe("function");
      expect(typeof build).toBe("function");

      return null;
    };

    render(
      <MetadataProvider>
        <TestComponent />
      </MetadataProvider>
    );
  });

  it("throws an error when useMetadata is used outside of MetadataProvider", () => {
    const TestComponent = () => {
      useMetadata();
      return null;
    };

    expect(() => render(<TestComponent />)).toThrow(
      "useMetadata must be used within a MetadataProvider"
    );
  });
});
