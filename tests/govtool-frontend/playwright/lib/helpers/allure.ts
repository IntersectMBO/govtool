import { allure } from "allure-playwright";
import { isMobileWidth } from "./mobile";
import { test } from "@playwright/test";

// Reads the project's viewport: launching a browser to ask a page left one
// running for every test, each traced and screenshotted by every later test.
export const setAllureEpic = async (groupName: string) => {
  const { viewport } = test.info().project.use;
  if (viewport && isMobileWidth(viewport.width)) {
    await allure.epic("6. Miscellaneous");
    await allure.story("6A. Should be accessible from mobile");
  } else {
    await allure.epic(groupName);
  }
};

export const setAllureStory = async (groupName: string) => {
  await allure.story(groupName);
};
