const PASSWORD_GENERATION_COST = 110000;

// For text that needs to be human readable.
// JSON, plaintext etc
const arrayBufferToString = (arrayBuffer) =>
  new Promise((resolve) => {
    const blob = new Blob([arrayBuffer]);
    const f = new FileReader();
    // eslint-disable-next-line fp/no-mutation
    f.onload = (e) => resolve(e.target.result);
    f.readAsText(blob);
  });

const stringToArrayBuffer = (string) =>
  new Promise((resolve) => {
    const blob = new Blob([string]);
    const f = new FileReader();
    // eslint-disable-next-line fp/no-mutation
    f.onload = (e) => resolve(e.target.result);
    f.readAsArrayBuffer(blob);
  });

// For iv + ciphertext
const arrayBufferToHexString = (arrayBuffer) =>
  new Uint8Array(arrayBuffer).reduce(
    (acc, v) => acc + v.toString(16).padStart(2, "0"),
    ""
  );

const hexStringToArrayBuffer = (hex) => {
  const parse = (acc, str) => {
    const start = str.slice(0, 2);
    const tail = str.substr(2);

    return start ? parse(acc.concat(parseInt(start, 16)), tail) : acc;
  };
  return new Uint8Array(parse([], hex));
};

const encrypt = async ({ content, key }) => {
  const encryptionKey = await crypto.subtle.importKey(
    "jwk",
    key,
    { name: "AES-CBC" },
    false,
    ["encrypt"]
  );

  const iv = await crypto.getRandomValues(new Uint8Array(16));

  const ciphertext = await crypto.subtle.encrypt(
    {
      name: "AES-CBC",
      iv,
    },
    encryptionKey,
    await stringToArrayBuffer(content)
  );

  return {
    iv: arrayBufferToHexString(iv),
    ciphertext: arrayBufferToHexString(ciphertext),
  };
};

const decrypt = async ({ iv, ciphertext, key }) => {
  const encryptionKey = await crypto.subtle.importKey(
    "jwk",
    key,
    { name: "AES-CBC" },
    false,
    ["decrypt"]
  );

  const plaintextContent = await crypto.subtle.decrypt(
    { name: "AES-CBC", iv: hexStringToArrayBuffer(iv) },
    encryptionKey,
    hexStringToArrayBuffer(ciphertext)
  );

  return arrayBufferToString(plaintextContent);
};

const keys = async ({ password, nonce }) => {
  const key = await crypto.subtle.importKey(
    "raw",
    await stringToArrayBuffer(password),
    { name: "PBKDF2" },
    false,
    ["deriveBits"]
  );

  const bits = await crypto.subtle.deriveBits(
    {
      name: "PBKDF2",
      salt: hexStringToArrayBuffer(nonce),
      iterations: PASSWORD_GENERATION_COST,
      hash: { name: "SHA-512" },
    },
    key,
    512
  );

  const n = bits.byteLength / 2;

  const serverKey = arrayBufferToHexString(bits.slice(0, n));

  const encryptionKey_ = await crypto.subtle.importKey(
    "raw",
    bits.slice(n),
    { name: "AES-CBC" },
    true,
    ["encrypt", "decrypt"]
  );

  const encryptionKey = await crypto.subtle.exportKey("jwk", encryptionKey_);

  return {
    serverKey,
    encryptionKey,
  };
};

const nonce = async () =>
  arrayBufferToHexString(await crypto.getRandomValues(new Uint8Array(8)));

module.exports = {
  keys,
  encrypt,
  decrypt,
  nonce,
};
