"use client";

import * as Sentry from "@sentry/nextjs";
import Error from "next/error";
import { useEffect } from "react";

export default function GlobalError({ error }) {
  useEffect(() => {
		// Use Sentry to capture and report the error as soon as the component mounts or the error changes.
		Sentry.captureException(error);
  }, [error]);

  return (
		<html>
			<body>
				{/* Render the Next.js Error component to display a generic error page to the user. */}
				<Error />
			</body>
		</html>
  );
}
