// Each test file starts from empty non-lookup tables.
import { truncateAll } from './test-db';

beforeAll(async () => {
  await truncateAll();
});
