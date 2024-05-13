# Metadata Service Package

🔍 This package provides a set of tools for managing metadata. It includes a `MetadataService` for handling metadata operations and a `MetadataProvider` for providing metadata context to React components.

![Statements](https://img.shields.io/badge/statements-96.33%25-brightgreen.svg?style=flat)
![Branches](https://img.shields.io/badge/branches-80%25-yellow.svg?style=flat)
![Functions](https://img.shields.io/badge/functions-90.9%25-brightgreen.svg?style=flat)
![Lines](https://img.shields.io/badge/lines-96.33%25-brightgreen.svg?style=flat)

## Getting started

First, install the package in your project:

```bash
yarn add metadata-service
```

### MetadataProvider

Wrap your application in the `MetadataProvider`:

```jsx
import { MetadataProvider } from "metadata-service";

function App() {
  return (
    <MetadataProvider>
      <YourComponent />
    </MetadataProvider>
  );
}
```

Now you can use the `useMetadata` hook to access metadata context in your components:

```jsx
import { useMetadata } from "metadata-service";

function YourComponent() {
  const metadata = useMetadata();

  // Use the metadata here...

  return <div>{/* Your component's JSX... */}</div>;
}
```

### MetadataService

You can also use the `MetadataService` directly to perform metadata operations:

```jsx
import { MetadataService } from "metadata-service";

const metadataService = new MetadataService({
  cip: CIP_Reference["0108"],
  hashAlgorithm: "blake2b-256",
  body: {
    title: "My title",
    abstract: "My abstract",
    motivation: "My motivation",
    rationale: "My rationale",
    references: [{ label: "some url", uri: "http://some.url" }],
  },
});

metadataService.initialize().then((service) => {
  // ...use the service here
});
```
