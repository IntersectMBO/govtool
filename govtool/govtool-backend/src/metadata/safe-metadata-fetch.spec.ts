import { EventEmitter } from 'node:events';
import { lookup } from 'node:dns/promises';
import { request } from 'node:http';
import { fetchMetadataText, safeLookup } from './safe-metadata-fetch';

jest.mock('node:dns/promises', () => ({ lookup: jest.fn() }));
jest.mock('node:http', () => ({ request: jest.fn() }));
jest.mock('node:https', () => ({ request: jest.fn() }));

describe('safe metadata requests', () => {
  afterEach(() => {
    jest.useRealTimers();
    jest.clearAllMocks();
  });
  it.each([
    'http://127.0.0.1/x',
    'http://169.254.169.254/',
    'http://10.0.0.1/',
    'http://[::1]/',
    'http://[::ffff:127.0.0.1]/',
    'http://localhost/',
    'file:///etc/passwd',
    'http://2130706433/',
  ])('blocks %s before opening a socket', async (url) => {
    await expect(fetchMetadataText(url, {})).rejects.toMatchObject({
      code: 'URL_BLOCKED',
    });
    expect(request).not.toHaveBeenCalled();
  });
  it('rejects private DNS answers at socket lookup, including mixed answers', async () => {
    jest.mocked(lookup).mockResolvedValue([
      { address: '8.8.8.8', family: 4 },
      { address: '127.0.0.1', family: 4 },
    ] as never);
    const callback = jest.fn();
    safeLookup('example.org', { all: true }, callback);
    await Promise.resolve();
    expect(callback).toHaveBeenCalledWith(
      expect.objectContaining({ code: 'URL_BLOCKED' }),
      '',
      0,
    );
  });
  it('passes permitted DNS answers to the socket', async () => {
    jest.mocked(lookup).mockResolvedValue({ address: '8.8.8.8', family: 4 });
    const callback = jest.fn();
    safeLookup('example.org', {}, callback);
    await Promise.resolve();
    expect(callback).toHaveBeenCalledWith(null, '8.8.8.8', 4);
  });
  function connection() {
    const req = Object.assign(new EventEmitter(), {
      end: jest.fn(),
      destroy: jest.fn((error?: Error) => {
        if (error) req.emit('error', error);
      }),
    });
    jest.mocked(request).mockReturnValue(req as never);
    const response = Object.assign(new EventEmitter(), {
      statusCode: 200,
      destroy: jest.fn(),
    });
    return { req, response };
  }
  it('blocks redirects without issuing another request', async () => {
    const { req, response } = connection();
    response.statusCode = 302;
    const result = fetchMetadataText('http://example.org', {});
    req.emit('response', response);
    await expect(result).rejects.toMatchObject({ code: 'URL_BLOCKED' });
    expect(request).toHaveBeenCalledTimes(1);
    expect(response.destroy).toHaveBeenCalled();
  });
  it('bounds streaming response size', async () => {
    const { req, response } = connection();
    const result = fetchMetadataText('http://example.org', {});
    req.emit('response', response);
    response.emit('data', Buffer.alloc(1024 * 1024 + 1));
    await expect(result).rejects.toMatchObject({ code: 'URL_NOT_FOUND' });
    expect(response.destroy).toHaveBeenCalled();
  });
  it('times out stalled requests', async () => {
    jest.useFakeTimers();
    const { req } = connection();
    const result = expect(
      fetchMetadataText('http://example.org', {}),
    ).rejects.toMatchObject({ code: 'URL_NOT_FOUND' });
    await jest.advanceTimersByTimeAsync(10_000);
    await result;
    expect(req.destroy).toHaveBeenCalled();
  });
  it('returns public response bytes unchanged for hashing', async () => {
    const { req, response } = connection();
    const result = fetchMetadataText('http://example.org', {});
    req.emit('response', response);
    response.emit('data', Buffer.from('{"body":'));
    response.emit('data', Buffer.from('{}}'));
    response.emit('end');
    await expect(result).resolves.toBe('{"body":{}}');
    expect(request).toHaveBeenCalledWith(
      expect.any(URL),
      expect.objectContaining({ lookup: safeLookup, agent: false }),
    );
  });
});
