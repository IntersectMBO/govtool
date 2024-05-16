export class Logger {
  static success(msg: string) {
    console.debug(`\x1b[32m✔ ${msg}\x1b[0m`); // Green color
  }

  static fail(msg) {
    console.debug(`\x1b[33m✖ ${msg}\x1b[0m`); // Orange color
  }

  static info(msg: string) {
    console.debug(`\x1b[36mℹ ${msg}\x1b[0m`); // Cyan color
  }
}
