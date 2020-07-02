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

window.navigator.serviceWorker.register("/sw.js").then(() => {
  const updateOnlineStatus = (event) => {
    switch (event.type) {
      case "online":
        return app.ports.online.send(true);
      case "offline":
        return app.ports.online.send(false);
    }
  };

  const now = new Date();

  const auth = localStorage.getItem("authed");

  const flags = {
    auth,
    month: now.getMonth(),
    year: now.getFullYear(),
    online: navigator.onLine,
    href: location.href,
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

  app.ports.clearAuth.subscribe(() => localStorage.removeItem("authed"));

  app.ports.buy.subscribe(({ email, annual }) => {
    loadStripe(stripeProjectId).then((stripe) => {
      const plan = annual ? stripeAnnual : stripeMonthly;

      stripe
        .redirectToCheckout({
          items: [{ plan, quantity: 1 }],
          customerEmail: email,
          successUrl: location.origin + "/payment-success",
          cancelUrl: location.origin,
        })
        .then(console.log)
        .catch(console.error);
    });
  });

  app.ports.saveAuth.subscribe((key) =>
    localStorage.setItem("authed", JSON.stringify(key))
  );

  window.addEventListener("online", updateOnlineStatus);
  window.addEventListener("offline", updateOnlineStatus);
});
