"use client";
import { MetadataService, CIP_Reference } from "govtool-submission-tool";

async function getJSONLD() {
  const metadataService = new MetadataService({
    cip: CIP_Reference["0108"],
    hashAlgorithm: "blake2b-256",
    body: {
      title: "Web App Boilerplate",
      description: "Web App Boilerplate",
      abstract: "This is a web app boilerplate.",
      motivation:
        "This web app boilerplate is designed to help you get started with your web app project.",
      rationale:
        "This web app boilerplate is designed to help you get started with your web app project.",
      references: [{ label: "Web App Boilerplate", uri: "http://example.com" }],
    },
  });

  return (await metadataService.initialize()).jsonld;
}

export default function Metadata() {
  const handleDownload = async () => {
    const jsonld = await getJSONLD();
    const jsonDataStr = JSON.stringify(jsonld, null, 2);
    const blob = new Blob([jsonDataStr], { type: "application/json" });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "metadata.json";
    document.body.appendChild(a);
    a.click();
    window.URL.revokeObjectURL(url);
  };
  return (
    <main>
      <button onClick={handleDownload}>Download metadata</button>
    </main>
  );
}
