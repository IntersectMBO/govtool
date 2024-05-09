require("@testing-library/jest-dom");

const { TextDecoder, TextEncoder } = require("text-encoding");
global.TextDecoder = TextDecoder;
global.TextEncoder = TextEncoder;
