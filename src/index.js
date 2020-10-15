require("./index.css");
const { Elm } = require("./Main.elm");
const MobileDetect = require("mobile-detect");
const { loadStripe } = require("@stripe/stripe-js/pure");

loadStripe.setLoadParameters({ advancedFraudSignals: false });

/* eslint-disable no-undef */
const stripeProjectId = STRIPE_PROJECT_ID;
const stripeAnnual = STRIPE_ANNUAL;
const charge = STRIPE_CHARGE;
const coinbase = COINBASE_CHECKOUT;
/* eslint-enable no-undef */

const LS_KEY = "KEY_A";

const mobileDetect = new MobileDetect(window.navigator.userAgent);

const isNewIpad = Boolean(
  window.navigator.userAgent.match(/Mac/) &&
    window.navigator.maxTouchPoints &&
    window.navigator.maxTouchPoints > 2
);

const isMobile = Boolean(mobileDetect.mobile()) || isNewIpad;

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
    window.Response &&
    window.TextEncoder &&
    window.TextDecoder &&
    [
      crypto.getRandomValues,
      crypto.subtle.importKey,
      crypto.subtle.exportKey,
      crypto.subtle.encrypt,
      crypto.subtle.decrypt,
      crypto.subtle.deriveBits,
    ].every(isFn)
);

const boot = (swActive) => {
  const now = new Date();

  const key = localStorage.getItem(LS_KEY);

  const flags = {
    month: now.getMonth(),
    year: now.getFullYear(),
    screen: {
      width: window.innerWidth,
      height: window.innerHeight,
    },
    isMobile,
    swActive,
    charge,
    href: location.href,
    key: swActive ? key : null,
    coinbase,
  };

  if (!key) {
    window.history.replaceState({}, "", "/");
  }

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

  app.ports.clearState.subscribe(() => localStorage.removeItem(LS_KEY));

  app.ports.buy.subscribe((email) =>
    loadStripe(stripeProjectId)
      .then((stripe) =>
        stripe.redirectToCheckout({
          items: [{ plan: stripeAnnual, quantity: 1 }],
          customerEmail: email,
          successUrl: location.origin + "/?payment_result=true",
          cancelUrl: location.origin + "/?payment_result=false",
        })
      )
      .then(console.log)
      .catch((e) => {
        console.error(e);
        app.ports.paymentFail.send(null);
      })
  );

  app.ports.saveState.subscribe((state) =>
    localStorage.setItem(LS_KEY, JSON.stringify(state))
  );
};

if (swEnabled && cryptoEnabled && asyncEnabled) {
  window.navigator.serviceWorker.register("/sw.js").then(
    () => boot(true),
    () => /* registration failure */ boot(false)
  );
} else {
  boot(false);
}
