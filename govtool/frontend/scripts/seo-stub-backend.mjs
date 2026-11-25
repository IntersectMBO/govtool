// A stand-in for govtool-backend's /seo routes, for scripts/check-seo-nginx.sh.
// The X-Stub-Mode request header picks a behaviour: ok (default), slow (2 s),
// error (500) or empty (200, no body). nginx passes the page request's headers
// on to its SSI subrequests, so a test sets the mode on the page request.
import http from "node:http";

const PNG = Buffer.from("89504e470d0a1a0a0000000d49484452", "hex");

const escape = (s) =>
  s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);

http
  .createServer((req, res) => {
    const mode = req.headers["x-stub-mode"] ?? "ok";
    const path = req.headers["x-seo-path"] ?? "";
    const send = (status, type, body, headers = {}) => {
      res.writeHead(status, { "Content-Type": type, ...headers });
      res.end(body);
    };
    const handle = () => {
      // The real backend refuses calls that came through a proxy, so the
      // frontend's nginx must strip the gateway's headers.
      const proxied = ["x-forwarded-for", "x-real-ip", "forwarded", "x-forwarded-host", "x-forwarded-proto"]
        .some((h) => req.headers[h] !== undefined);
      if (proxied) return send(404, "text/plain", "PROXIED");
      if (mode === "error") return send(500, "text/html", "STUB ERROR");
      if (mode === "empty") return send(200, "text/html", "");
      if (req.url === "/seo/head") {
        return send(200, "text/html; charset=utf-8", `<title data-seo>Stub ${escape(path)}</title>`);
      }
      if (req.url === "/seo/body") {
        return send(200, "text/html; charset=utf-8", "<main data-seo><h1>Stub body</h1></main>");
      }
      if (req.url === "/seo/sitemap.xml") {
        return send(200, "application/xml", "<urlset>stub</urlset>");
      }
      if (req.url.endsWith("/000000000000.png")) {
        return send(301, "text/plain", "", { Location: "/og/action/a/0123456789ab.png" });
      }
      if (req.url.startsWith("/seo/image/")) {
        return send(200, "image/png", PNG, {
          "Cache-Control": "public, max-age=31536000, immutable",
        });
      }
      return send(404, "text/plain", "not found");
    };
    if (mode === "slow") setTimeout(handle, 2000);
    else handle();
  })
  .listen(9999);
