import { z } from "zod";

export const CIP0108ValidationSchema = z.object({
  title: z.string().max(80),
  abstract: z.string().max(2500),
  motivation: z.string(),
  rationale: z.string(),
  references: z.array(
    z.object({
      label: z.string(),
      uri: z.string().url(),
    })
  ),
});
