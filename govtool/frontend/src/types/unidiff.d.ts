declare module "unidiff" {
  export function diffLines(oldStr: string, newStr: string): string[];
  export function formatLines(lines: string[]): string;
}
