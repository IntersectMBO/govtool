export const openInNewTab = (url: string) => {
  // Ensure the URL is absolute
  const fullUrl =
    url.startsWith("http://") || url.startsWith("https://")
      ? url
      : `https://${url}`;

  const newWindow = window.open(fullUrl, "_blank", "noopener,noreferrer");
  if (newWindow) newWindow.opener = null;
};
