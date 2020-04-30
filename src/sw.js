const crypto = require("./crypto.js");

const jsonResponse = (a) => new Response(JSON.stringify(a));

const make400 = (txt) =>
  new Response(JSON.stringify({ errors: [txt] }), { status: 400 });

const handlers = async (request) => {
  const action = request.url.substring(self.location.origin.length + 1);

  switch (action) {
    case "nonce": {
      return jsonResponse(await crypto.nonce());
    }
    case "keys": {
      const { password, nonce } = await request.json();

      return jsonResponse(await crypto.keys(password, nonce));
    }
    case "decrypt": {
      const { content, key } = await request.json();

      return jsonResponse(await crypto.decrypt(content, key));
    }
    case "encrypt": {
      const { content, key } = await request.json();

      return jsonResponse(await crypto.encrypt(content, key));
    }
    default: {
      return make400("unmatched url");
    }
  }
};

self.addEventListener("activate", (event) => {
  // Ensure all subsequent requests are processed
  // by the "fetch" listener.
  event.waitUntil(self.clients.claim());
});

self.addEventListener("install", (event) => event);

self.addEventListener("fetch", (e) =>
  e.request.method === "CRYPTO" ? e.respondWith(handlers(e.request)) : e
);
