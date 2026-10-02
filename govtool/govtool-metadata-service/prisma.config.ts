import "dotenv/config";
import { defineConfig, env } from "prisma/config";
import { readFileSync } from "node:fs";

/**
 * Mirror of src/config/secrets.ts for the Prisma CLI: Swarm secrets mount
 * at /run/secrets/<lowercase name>; `<name>_FILE` overrides the path.
 * Pre-populates process.env so env() below resolves a file-backed value.
 */
function loadSecretIntoEnv(name: string): void {
  const direct = process.env[name];
  if (direct !== undefined && direct.trim() !== "") return;
  const file =
    process.env[`${name}_FILE`] ?? `/run/secrets/${name.toLowerCase()}`;
  try {
    const value = readFileSync(file, "utf8").replace(/\r?\n$/, "");
    if (value.trim() !== "") process.env[name] = value;
  } catch {
    /* unset: the Prisma CLI reports the missing variable */
  }
}

loadSecretIntoEnv("DATABASE_URL");
loadSecretIntoEnv("SHADOW_DATABASE_URL");

export default defineConfig({
  schema: "prisma/schema.prisma",
  migrations: {
    path: "prisma/migrations",
  },
  datasource: {
    url: env("DATABASE_URL"),
    ...(process.env.SHADOW_DATABASE_URL
      ? { shadowDatabaseUrl: process.env.SHADOW_DATABASE_URL }
      : {}),
  },
});
