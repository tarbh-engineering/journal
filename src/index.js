const { Elm } = require("./Main.elm");

const { loadStripe } = require("@stripe/stripe-js/pure");
loadStripe.setLoadParameters({ advancedFraudSignals: false });

const MobileDetect = require("mobile-detect");

const md = new MobileDetect(window.navigator.userAgent);

const isNewIpad = Boolean(
  window.navigator.userAgent.match(/Mac/) &&
    window.navigator.maxTouchPoints &&
    window.navigator.maxTouchPoints > 2
);

const isMobile = Boolean(md.mobile()) || isNewIpad;

require("./index.css");

/* eslint-disable no-undef */
const stripeProjectId = STRIPE_PROJECT_ID;
const stripeAnnual = STRIPE_ANNUAL;
const stripeMonthly = STRIPE_MONTHLY;
/* eslint-enable no-undef */

const CRYPTO_KEY = "KEY_A";

const isFn = (x) => typeof x === "function";

const asyncEnabled = (() => {
  try {
    eval("async () => {}");
  } catch (e) {
    return false;
  }

  return true;
})();

const swEnabled = Boolean(
  window.navigator.serviceWorker &&
    isFn(window.navigator.serviceWorker.register)
);

const cryptoEnabled = Boolean(
  window.crypto &&
    window.crypto.subtle &&
    [
      crypto.getRandomValues,
      crypto.subtle.importKey,
      crypto.subtle.exportKey,
      crypto.subtle.encrypt,
      crypto.subtle.decrypt,
      crypto.subtle.deriveBits,
    ].every(isFn)
);

const now = new Date();

const flags = {
  month: now.getMonth(),
  year: now.getFullYear(),
  screen: {
    width: window.innerWidth,
    height: window.innerHeight,
  },
  isMobile,
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

app.ports.log.subscribe(console.log);

app.ports.clearAuth.subscribe(() => localStorage.removeItem(CRYPTO_KEY));

const x = () =>
  app.ports.buy.subscribe(({ email, annual }) =>
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
      .catch((e) => {
        console.error(e);
        app.ports.paymentFail.send(null);
      })
  );

app.ports.saveAuth.subscribe((key) =>
  localStorage.setItem(CRYPTO_KEY, JSON.stringify(key))
);

const boot = (swActive) =>
  app.ports.boot.send({
    href: location.href,
    key: localStorage.getItem(CRYPTO_KEY),
    swActive,
  });

if (swEnabled && cryptoEnabled && asyncEnabled) {
  window.navigator.serviceWorker.register("/sw.js").then(
    () => boot(true),
    () => /* registration failure */ boot(false)
  );
} else {
  boot(false);
}
