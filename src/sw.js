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
      const body = await request.json();

      return jsonResponse(await crypto.keys(body));
    }
    case "decrypt": {
      const body = await request.json();

      return jsonResponse(await crypto.decrypt(body));
    }
    case "encrypt": {
      const body = await request.json();

      return jsonResponse(await crypto.encrypt(body));
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
