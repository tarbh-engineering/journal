const crypto = require("../src/crypto.js");

(async () => {
  const password = String(Date.now());
  const data = String(Date.now());

  const nonce = await crypto.nonce();

  const keys = await crypto.keys({ password, nonce });

  const { iv, ciphertext } = await crypto.encrypt({
    content: data,
    key: keys.encryptionKey,
  });

  const keys2 = await crypto.keys({ password, nonce });

  const dec = await crypto.decrypt({
    iv,
    ciphertext,
    key: keys2.encryptionKey,
  });

  return data === dec;
})()
  .then(console.log)
  .catch(console.error);
