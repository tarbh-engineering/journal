const crypto = require("../src/crypto.js");

(async () => {
  const password = String(Date.now());
  const data = String(Date.now());

  const nonce = await crypto.nonce();

  const keys = await crypto.keys(password, nonce);

  const enc = await crypto.encrypt(data, keys.encryptionKey);

  const keys2 = await crypto.keys(password, nonce);

  const dec = await crypto.decrypt(enc, keys2.encryptionKey);

  return data === dec;
})()
  .then(console.log)
  .catch(console.error);
