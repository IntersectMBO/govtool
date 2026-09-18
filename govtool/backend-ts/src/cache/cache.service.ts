import { Injectable,Logger } from "@nestjs/common";


import { ConfigService } from "src/config/config.service";

type CacheEntry<T> = {
    expiresAt: number;
    value: Promise<T>;
    refreshing: boolean;
};

@Injectable()
export class CacheService {
    private readonly logger = new Logger(CacheService.name);
    private readonly cache = new Map<string, CacheEntry<unknown>>();

    constructor(private readonly configSerivce: ConfigService){}

    getOrSet<T>(
        namespace: string,
        key: unknown,
        action: () => Promise<T>,
        ttlSeconds = this.defaultTtlSeconds(),
        ): Promise<T> {
        const cacheKey = this.toCacheKey(namespace, key);
        const now = Date.now();
        const entry = this.cache.get(cacheKey) as CacheEntry<T> | undefined;

        if (entry && entry.expiresAt > now) {
            this.touch(cacheKey, entry);
            return entry.value;
        }

        const value = action().catch((error) => {
            if (this.cache.get(cacheKey)?.value === value) this.cache.delete(cacheKey);
            throw error;
        });

        this.store(cacheKey, {
            expiresAt: now + ttlSeconds * 1000,
            value,
            refreshing: false,
        });

        return value;
    }


    async getOrSetStaleWhileRevalidate<T>(
        namespace: string,
        key: unknown,
        action: ()=> Promise<T>,
        ttlSeconds = this.defaultTtlSeconds(),
    ): Promise<T> {
        const cacheKey = this.toCacheKey(namespace,key);
        const entry = this.cache.get(cacheKey) as CacheEntry<T> | undefined;
        const now =Date.now();

        if(!entry){
            return this.getOrSet(namespace,key,action,ttlSeconds);
        }
        this.touch(cacheKey, entry);
        if(entry.expiresAt > now){
            return entry.value;
        }
        if (!entry.refreshing) {
            entry.refreshing = true;
            void action()
                .then((value)=> {
                    if (this.cache.get(cacheKey) === entry) this.set(namespace, key, value, ttlSeconds);
                })
                .catch((error)=>{
                    this.logger.error(
                        `Failed to refresh cache ${cacheKey}`,
                        error instanceof Error ? error.stack: String(error),
                    );
                })
                .finally(()=>{
                    const latest = this.cache.get(cacheKey);
                    if (latest === entry) {
                        latest.refreshing = false;
                    }
                });
        }
        return entry.value
    }

    async refresh<T>(
        namespace: string,
        key: unknown,
        action: () => Promise<T>,
        ttlSeconds = this.defaultTtlSeconds(),
    ): Promise<T> {
        const value = await action();
        this.set(namespace,key,value,ttlSeconds);
        return value;
    }

    set<T>(
        namespace: string,
        key: unknown,
        value: T,
        ttlSeconds = this.defaultTtlSeconds(),
    ): void {
        const cacheKey = this.toCacheKey(namespace,key);

        this.store(cacheKey, {
            expiresAt: Date.now()+ttlSeconds*1000,
            value: Promise.resolve(value),
            refreshing: false,
        });
    }

    delete(namespace: string, key:unknown): void {
        this.cache.delete(this.toCacheKey(namespace, key));
    }

    clear():void{
        this.cache.clear();
    }

    defaultTtlSeconds():number {
        return this.configSerivce.get().cacheDurationSeconds;
    }

    drepListTtlSeconds(): number {
        return this.configSerivce.get().drepListCacheDurationSeconds;
    }

    private touch(key: string, entry: CacheEntry<unknown>): void {
        this.cache.delete(key);
        this.cache.set(key, entry);
    }

    private store(key: string, entry: CacheEntry<unknown>): void {
        this.touch(key, entry);
        const maxEntries = this.configSerivce.get().cacheMaxEntries;
        while (this.cache.size > maxEntries) {
            this.cache.delete(this.cache.keys().next().value!);
        }
    }

    private toCacheKey(namespace: string, key:unknown): string {
        return `${namespace}:${JSON.stringify(key)}`;
    }
}