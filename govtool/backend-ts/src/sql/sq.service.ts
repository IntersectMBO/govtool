import { Injectable } from "@nestjs/common";
import * as fs from 'fs';
import * as path from 'path';

@Injectable()
export class SqlService {
    private readonly cache = new Map<string, string>();
    //check if file is in cache
    load(fileName: string): string {
        const cached = this.cache.get(fileName);

        if(cached) {
            return cached;
        } 

        const filePath = path.resolve(process.cwd(), 'sql', fileName);
        const sql = fs.readFileSync(filePath, 'utf8');

        this.cache.set(fileName, sql);
        return sql;
    }
}