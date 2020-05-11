const { Elm } = require("./Main.elm");
const { loadStripe } = require("@stripe/stripe-js");

require("./index.css");

/* eslint-disable no-undef */
const stripeProjectId = STRIPE_PROJECT_ID;
const stripeAnnual = STRIPE_ANNUAL;
const stripeMonthly = STRIPE_MONTHLY;
/* eslint-enable no-undef */

window.navigator.serviceWorker.register("/sw.js").then(() => {
  const auth = (() => {
    try {
      return JSON.parse(localStorage.getItem("APP"));
    } catch (e) {
      return null;
    }
  })();

  const updateOnlineStatus = (event) => {
    switch (event.type) {
      case "online":
        return app.ports.online.send(true);
      case "offline":
        return app.ports.online.send(false);
    }
  };

  const now = new Date();

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

  app.ports.clearAuth.subscribe(() => localStorage.removeItem("APP"));

  app.ports.buy.subscribe(async ({ email, annual }) => {
    const stripe = await loadStripe(stripeProjectId);

    stripe
      .redirectToCheckout({
        items: [{ plan: annual ? stripeAnnual : stripeMonthly, quantity: 1 }],
        customerEmail: email,
        successUrl: location.origin + "/payment-success",
        cancelUrl: location.origin,
      })
      .then(console.log);
  });

  app.ports.saveAuth.subscribe((x) =>
    localStorage.setItem("APP", JSON.stringify(x))
  );

  window.addEventListener("online", updateOnlineStatus);
  window.addEventListener("offline", updateOnlineStatus);
});
