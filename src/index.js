const { Elm } = require("./Main.elm");

const { loadStripe } = require("@stripe/stripe-js/pure");
loadStripe.setLoadParameters({ advancedFraudSignals: false });

const MobileDetect = require("mobile-detect");

const md = new MobileDetect(window.navigator.userAgent);

const isMobile = Boolean(md.mobile());

require("./index.css");

/* eslint-disable no-undef */
const stripeProjectId = STRIPE_PROJECT_ID;
const stripeAnnual = STRIPE_ANNUAL;
const stripeMonthly = STRIPE_MONTHLY;
/* eslint-enable no-undef */

const CRYPTO_KEY = "KEY_A";

const isFn = (x) => typeof x === "function";

const swEnabled =
  window.navigator.serviceWorker &&
  isFn(window.navigator.serviceWorker.register);

const cryptoEnabled = [
  window.crypto,
  isFn(crypto.getRandomValues),
  crypto.subtle,
  isFn(crypto.subtle.importKey),
  isFn(crypto.subtle.exportKey),
  isFn(crypto.subtle.encrypt),
  isFn(crypto.subtle.decrypt),
  isFn(crypto.subtle.deriveBits),
].every((x) => x);

const now = new Date();

const flags = {
  month: now.getMonth(),
  year: now.getFullYear(),
  online: navigator.onLine,
  screen: {
    width: window.innerWidth,
    height: window.innerHeight,
  },
  isMobile,
  swEnabled,
  cryptoEnabled,
};

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags,
});

window.addEventListener("popstate", () =>
  app.ports.onUrlChange.send(location.href)
);

app.ports.pushUrl.subscribe((url) => {
  history.pushState({}, "", url);
  app.ports.onUrlChange.send(location.href);
});

const updateOnlineStatus = (event) => {
  switch (event.type) {
    case "online":
      return app.ports.online.send(true);
    case "offline":
      return app.ports.online.send(false);
  }
};

app.ports.log.subscribe(console.log);

app.ports.clearAuth.subscribe(() => localStorage.removeItem("authed"));

app.ports.buy.subscribe(({ email, annual }) => {
  loadStripe(stripeProjectId)
    .then((stripe) =>
      stripe.redirectToCheckout({
        items: [{ plan: annual ? stripeAnnual : stripeMonthly, quantity: 1 }],
        customerEmail: email,
        successUrl: location.origin + "/payment-success",
        cancelUrl: location.origin,
      })
    )
    .then(console.log)
    .catch(console.error);
});

app.ports.saveAuth.subscribe((key) =>
  localStorage.setItem(CRYPTO_KEY, JSON.stringify(key))
);

window.addEventListener("online", updateOnlineStatus);
window.addEventListener("offline", updateOnlineStatus);

if (swEnabled) {
  window.navigator.serviceWorker.register("/sw.js").then(() =>
    app.ports.boot.send({
      href: location.href,
      key: localStorage.getItem(CRYPTO_KEY),
    })
  );
}
