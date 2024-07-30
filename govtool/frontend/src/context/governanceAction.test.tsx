import { useEffect } from "react";
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import {
  GovernanceActionProvider,
  useGovernanceActions,
} from "./governanceAction";

const resultMetadataBody = {
  abstract: {
    "@value": "Test",
  },
  motivation: {
    "@value": "Test",
  },
  rationale: {
    "@value": "Test",
  },
  references: [
    {
      "@type": "Other",
      label: {
        "@value": "Test",
      },
      uri: {
        "@value": "http://example.com",
      },
    },
  ],
  title: {
    "@value": "Test",
  },
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
            "b84e9890a34d6e59a983cf8f695214162893de5fc5310b12b75f4fe3dab0d7ab",
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
