import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import request from 'supertest';
import { Server } from 'http';
import { server as appServer } from '../src/index';
import { prisma } from '../src/config/db';
import { blake2b } from 'libcardano'; // Import blake2b

let server: Server;

beforeAll(async () => {
  await prisma.$connect();
  server = appServer.listen(0);
});

afterAll(async () => {
  await new Promise<void>((resolve) => server.close(() => resolve()));
  await prisma.$disconnect();
});

beforeEach(async () => {
  await prisma.datastore.deleteMany({});
});

describe('PUT and GET /api/data/:identifier (hash)', () => {
  const testData = {
    body: { comment: 'This is just a test update by a non-malicious user' },
    hashAlgorithm: 'blake2b-256',
  };
  const testDataHash = blake2b.hash32(Buffer.from(JSON.stringify(testData))).toString('hex');

  it('should store data with PUT using hash as identifier and retrieve it with GET', async () => {
    // PUT request to store the data using hash as identifier
    const putResponse = await request(server)
      .put(`/api/data/${testDataHash}`)
      .send(testData)
      .set('Content-Type', 'application/json');

    expect(putResponse.status).toBe(200);
    expect(putResponse.body.message).toBe('Data stored successfully');
    expect(putResponse.body.identifier).toBe(testDataHash);
    expect(putResponse.body.hash).toBe(testDataHash);

    // GET request to retrieve the data using hash as identifier
    const getResponse = await request(server).get(`/api/data/${testDataHash}`);

    expect(getResponse.status).toBe(200);
    expect(JSON.parse(getResponse.text)).toEqual(testData);
  });

  it('should store data with PUT using filename as identifier and retrieve it with GET', async () => {
    const filename = 'test-file-123';
    const fileContent = { message: 'This is a test file content' };
    const fileContentHash = blake2b.hash32(Buffer.from(JSON.stringify(fileContent))).toString('hex');

    // PUT request to store the data using filename as identifier
    const putResponse = await request(server)
      .put(`/api/data/${filename}`)
      .send(fileContent)
      .set('Content-Type', 'application/json');

    expect(putResponse.status).toBe(200);
    expect(putResponse.body.message).toBe('Data stored successfully');
    expect(putResponse.body.identifier).toBe(filename);
    expect(putResponse.body.hash).toBe(fileContentHash);

    // GET request to retrieve the data using filename as identifier
    const getResponse = await request(server).get(`/api/data/${filename}`);

    expect(getResponse.status).toBe(200);
    expect(JSON.parse(getResponse.text)).toEqual(fileContent);

    // GET request to retrieve the data using hash as identifier
    const getResponseByHash = await request(server).get(`/api/data/${fileContentHash}`);
    expect(getResponseByHash.status).toBe(200);
    expect(JSON.parse(getResponseByHash.text)).toEqual(fileContent);
  });

  it('should store plain text data without Content-Type header and retrieve it correctly', async () => {
    const filename = 'plain-text-no-type';
    const textContent = 'This is some plain text data without a content type.';
    const textContentHash = blake2b.hash32(Buffer.from(textContent)).toString('hex');

    // PUT request without Content-Type header
    const putResponse = await request(server)
      .put(`/api/data/${filename}`)
      .send(textContent); // .set('Content-Type', ...) is omitted

    expect(putResponse.status).toBe(200);
    expect(putResponse.body.message).toBe('Data stored successfully');
    expect(putResponse.body.identifier).toBe(filename);
    expect(putResponse.body.hash).toBe(textContentHash);

    // GET request to retrieve the data
    const getResponse = await request(server).get(`/api/data/${filename}`);

    expect(getResponse.status).toBe(200);
    expect(getResponse.text).toBe(textContent); // Use .text for plain text response
    // Content-Type assertion removed as per user's clarification
  });

  it('should store JSON data with application/json Content-Type and retrieve it correctly', async () => {
    const filename = 'json-data-with-type';
    const jsonData = { key: 'value', number: 123 };
    const jsonDataString = JSON.stringify(jsonData);
    const jsonDataHash = blake2b.hash32(Buffer.from(jsonDataString)).toString('hex');

    // PUT request with application/json Content-Type
    const putResponse = await request(server)
      .put(`/api/data/${filename}`)
      .send(jsonData)
      .set('Content-Type', 'application/json');

    expect(putResponse.status).toBe(200);
    expect(putResponse.body.message).toBe('Data stored successfully');
    expect(putResponse.body.identifier).toBe(filename);
    expect(putResponse.body.hash).toBe(jsonDataHash);

    // GET request to retrieve the data
    const getResponse = await request(server).get(`/api/data/${filename}`);

    expect(getResponse.status).toBe(200);
    expect(JSON.parse(getResponse.text)).toEqual(jsonData); // Parse .text for JSON response
    // Content-Type assertion removed as per user's clarification
  });

  it('should store plain text data with text/plain Content-Type and retrieve it correctly', async () => {
    const filename = 'plain-text-with-type';
    const textContent = 'Another piece of plain text data with explicit content type.';
    const textContentHash = blake2b.hash32(Buffer.from(textContent)).toString('hex');

    // PUT request with text/plain Content-Type
    const putResponse = await request(server)
      .put(`/api/data/${filename}`)
      .send(textContent)
      .set('Content-Type', 'text/plain');

    expect(putResponse.status).toBe(200);
    expect(putResponse.body.message).toBe('Data stored successfully');
    expect(putResponse.body.identifier).toBe(filename);
    expect(putResponse.body.hash).toBe(textContentHash);

    // GET request to retrieve the data
    const getResponse = await request(server).get(`/api/data/${filename}`);

    expect(getResponse.status).toBe(200);
    expect(getResponse.text).toBe(textContent); // Use .text for plain text response
    // Content-Type assertion removed as per user's clarification
  });
});
