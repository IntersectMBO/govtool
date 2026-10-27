import { Page } from "@playwright/test";

export function isMobileWidth(width: number) {
  return width <= 414;
}

export function isMobile(page: Page) {
  const { width } = page.viewportSize();
  return isMobileWidth(width);
}

export async function openDrawer(page: Page) {
  await page.getByTestId("open-drawer-button").click();
}
