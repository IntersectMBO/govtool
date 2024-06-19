export function extractProposalIdFromUrl(url: string) {
  return parseInt(url.split("/").pop());
}
