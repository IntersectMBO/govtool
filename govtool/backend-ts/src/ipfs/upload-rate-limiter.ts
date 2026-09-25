type Window = { count: number; resetAt: number };

export type RateLimitDecision =
  | { allowed: true }
  | { allowed: false; retryAfterSeconds: number };

export type UploadRateLimits = {
  perClientLimit: number;
  globalLimit: number;
  windowSeconds: number;
  maxTrackedClients: number;
};

/**
 * Fixed-window limiter for anonymous IPFS uploads: one budget per client IP
 * and one shared budget for the whole instance, so a botnet spread over many
 * addresses is still capped.
 */
export class UploadRateLimiter {
  private readonly clients = new Map<string, Window>();
  private global: Window = { count: 0, resetAt: 0 };

  constructor(
    private readonly limits: UploadRateLimits,
    private readonly now: () => number = Date.now,
  ) {}

  consume(clientKey: string): RateLimitDecision {
    const now = this.now();
    const windowMs = this.limits.windowSeconds * 1_000;

    if (this.global.resetAt <= now) {
      this.global = { count: 0, resetAt: now + windowMs };
    }

    let client = this.clients.get(clientKey);
    if (!client || client.resetAt <= now) {
      if (!client && !this.makeRoom(now)) {
        return this.reject(this.global.resetAt, now);
      }
      client = { count: 0, resetAt: now + windowMs };
      this.clients.set(clientKey, client);
    }

    if (client.count >= this.limits.perClientLimit) {
      return this.reject(client.resetAt, now);
    }
    if (this.global.count >= this.limits.globalLimit) {
      return this.reject(this.global.resetAt, now);
    }

    client.count += 1;
    this.global.count += 1;
    return { allowed: true };
  }

  private makeRoom(now: number): boolean {
    if (this.clients.size < this.limits.maxTrackedClients) return true;

    for (const [key, window] of this.clients) {
      if (window.resetAt <= now) this.clients.delete(key);
    }
    // Fail closed rather than grow without bound.
    return this.clients.size < this.limits.maxTrackedClients;
  }

  private reject(resetAt: number, now: number): RateLimitDecision {
    return {
      allowed: false,
      retryAfterSeconds: Math.max(1, Math.ceil((resetAt - now) / 1_000)),
    };
  }
}
