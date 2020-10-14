const PASSWORD_GENERATION_COST = 110000;

// ArrayBuffer -> String
const toHex = (buffer) =>
  Array.from(new Uint8Array(buffer)).reduce(
    (acc, b) => acc + b.toString(16).padStart(2, "0"),
    ""
  );

// String -> ArrayBuffer
const fromHex = (hex) =>
  new Uint8Array(hex.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));

// () -> ArrayBuffer
const randomBits = () => crypto.getRandomValues(new Uint8Array(16));

// { content: String, key: JWK } -> { iv: String, ciphertext: String }
const encrypt = async ({ content, key }) => {
  const encryptionKey = await crypto.subtle.importKey(
    "jwk",
    key,
    { name: "AES-CBC" },
    false,
    ["encrypt"]
  );

  const iv = await randomBits();

  const ciphertext = await crypto.subtle.encrypt(
    {
      name: "AES-CBC",
      iv,
    },
    encryptionKey,
    new TextEncoder().encode(content)
  );

  return {
    iv: toHex(iv),
    ciphertext: toHex(ciphertext),
  };
};

// { iv: String, ciphertext: String, key: JWK } -> String
const decrypt = async ({ iv, ciphertext, key }) => {
  const encryptionKey = await crypto.subtle.importKey(
    "jwk",
    key,
    { name: "AES-CBC" },
    false,
    ["decrypt"]
  );

  const plaintextContent = await crypto.subtle.decrypt(
    { name: "AES-CBC", iv: fromHex(iv) },
    encryptionKey,
    fromHex(ciphertext)
  );

  return new TextDecoder().decode(plaintextContent);
};

// { password: String, nonce: String } -> { serverKey: String, encryptionKey: JWK }
const keys = async ({ password, nonce }) => {
  const key = await crypto.subtle.importKey(
    "raw",
    new TextEncoder().encode(password),
    { name: "PBKDF2" },
    false,
    ["deriveBits"]
  );

  const bits = await crypto.subtle.deriveBits(
    {
      name: "PBKDF2",
      salt: fromHex(nonce),
      iterations: PASSWORD_GENERATION_COST,
      hash: { name: "SHA-512" },
    },
    key,
    512
  );

  const n = bits.byteLength / 2;

  const serverKey = toHex(bits.slice(0, n));

  const encryptionKey = await crypto.subtle.importKey(
    "raw",
    bits.slice(n),
    { name: "AES-CBC" },
    true,
    ["encrypt", "decrypt"]
  );

  return {
    serverKey,
    encryptionKey: await crypto.subtle.exportKey("jwk", encryptionKey),
  };
};

// () -> String
const nonce = async () => toHex(await randomBits());

module.exports = {
  keys,
  encrypt,
  decrypt,
  nonce,
};
