import { useEffect } from "react";
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import {
  GovernanceActionProvider,
  useGovernanceActions,
} from "./governanceAction";

const resultMetadataBody = {
  abstract: "Test",
  motivation: "Test",
  rationale: "Test",
  references: [
    {
      "@type": "Other",
      label: "Test",
      uri: "http://example.com",
    },
  ],
  title: "Test",
};

describe("GovernanceActionProvider", () => {
  it("renders correctly", () => {
    render(
      <GovernanceActionProvider>
        <div>Test</div>
      </GovernanceActionProvider>,
    );
    expect(screen.getByText("Test")).toBeInTheDocument();
  });

  it("creates governance action JSON-LD", async () => {
    const metadata = {
      title: "Test",
      abstract: "Test",
      motivation: "Test",
      rationale: "Test",
      references: [{ uri: "http://example.com", label: "Test" }],
    };
    const TestComponent = () => {
      const { createGovernanceActionJsonLD } = useGovernanceActions();

      useEffect(() => {
        const test = async () => {
          const jsonld = await createGovernanceActionJsonLD(metadata);
          expect(jsonld).toBeDefined();
          expect(jsonld?.body).toStrictEqual(resultMetadataBody);
        };
        test();
      }, [createGovernanceActionJsonLD]);
      return null;
    };

    render(
      <GovernanceActionProvider>
        <TestComponent />
      </GovernanceActionProvider>,
    );
  });

  it("creates governance action jsonld and hash", async () => {
    const metadata = {
      title: "Test",
      abstract: "Test",
      motivation: "Test",
      rationale: "Test",
      references: [{ uri: "http://example.com", label: "Test" }],
    };
    const TestComponent = () => {
      const { createGovernanceActionJsonLD, createHash } =
        useGovernanceActions();

      useEffect(() => {
        const test = async () => {
          const jsonld = await createGovernanceActionJsonLD(metadata);
          expect(jsonld).toBeDefined();
          expect(jsonld?.body).toStrictEqual(resultMetadataBody);

          const hash = await createHash(jsonld!);
          expect(hash).toBeDefined();
          expect(hash).toBe(
            "9f7b2f83113e003d36231bd815cafbcc677c3a9513281d751a241720893cc877",
          );
        };
        test();
      }, [createGovernanceActionJsonLD]);
      return null;
    };

    render(
      <GovernanceActionProvider>
        <TestComponent />
      </GovernanceActionProvider>,
    );
  });
});
