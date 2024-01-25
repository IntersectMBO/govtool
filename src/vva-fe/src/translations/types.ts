import { en } from "./locales/en";

type JoinStringsWithDotNotation<
  S1 extends string,
  S2 extends string
> = `${S1}${"" extends S2 ? "" : "."}${S2}`;

type DeepKeys<ObjectToParse> = ObjectToParse extends Record<string, unknown>
  ? {
      [Key in keyof ObjectToParse]: JoinStringsWithDotNotation<
        Key & string,
        DeepKeys<ObjectToParse[Key]>
      >;
    }[keyof ObjectToParse]
  : "";

type DeepestKeys<ObjectToParse> = ObjectToParse extends Record<string, unknown>
  ? {
      [Key in keyof ObjectToParse]: JoinStringsWithDotNotation<
        Key & string,
        DeepKeys<ObjectToParse[Key]>
      >;
    }[keyof ObjectToParse]
  : "";

export type TranslationKey = DeepestKeys<(typeof en)["translation"]>;
