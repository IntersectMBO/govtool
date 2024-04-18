/*! For license information please see index.js.LICENSE.txt */
(() => {
  var t = {
      422: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.validateBasic = e.wNAF = void 0);
        const n = r(967),
          o = r(484),
          i = BigInt(0),
          s = BigInt(1);
        (e.wNAF = function (t, e) {
          const r = (t, e) => {
              const r = e.negate();
              return t ? r : e;
            },
            n = (t) => ({
              windows: Math.ceil(e / t) + 1,
              windowSize: 2 ** (t - 1),
            });
          return {
            constTimeNegate: r,
            unsafeLadder(e, r) {
              let n = t.ZERO,
                o = e;
              for (; r > i; )
                r & s && (n = n.add(o)), (o = o.double()), (r >>= s);
              return n;
            },
            precomputeWindow(t, e) {
              const { windows: r, windowSize: o } = n(e),
                i = [];
              let s = t,
                a = s;
              for (let t = 0; t < r; t++) {
                (a = s), i.push(a);
                for (let t = 1; t < o; t++) (a = a.add(s)), i.push(a);
                s = a.double();
              }
              return i;
            },
            wNAF(e, o, i) {
              const { windows: a, windowSize: f } = n(e);
              let u = t.ZERO,
                c = t.BASE;
              const l = BigInt(2 ** e - 1),
                h = 2 ** e,
                d = BigInt(e);
              for (let t = 0; t < a; t++) {
                const e = t * f;
                let n = Number(i & l);
                (i >>= d), n > f && ((n -= h), (i += s));
                const a = e,
                  p = e + Math.abs(n) - 1,
                  y = t % 2 != 0,
                  g = n < 0;
                0 === n ? (c = c.add(r(y, o[a]))) : (u = u.add(r(g, o[p])));
              }
              return { p: u, f: c };
            },
            wNAFCached(t, e, r, n) {
              const o = t._WINDOW_SIZE || 1;
              let i = e.get(t);
              return (
                i ||
                  ((i = this.precomputeWindow(t, o)),
                  1 !== o && e.set(t, n(i))),
                this.wNAF(o, i, r)
              );
            },
          };
        }),
          (e.validateBasic = function (t) {
            return (
              (0, n.validateField)(t.Fp),
              (0, o.validateObject)(
                t,
                { n: "bigint", h: "bigint", Gx: "field", Gy: "field" },
                { nBitLength: "isSafeInteger", nByteLength: "isSafeInteger" }
              ),
              Object.freeze({
                ...(0, n.nLength)(t.n, t.nBitLength),
                ...t,
                p: t.Fp.ORDER,
              })
            );
          });
      },
      377: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.twistedEdwards = void 0);
        const n = r(967),
          o = r(484),
          i = r(484),
          s = r(422),
          a = BigInt(0),
          f = BigInt(1),
          u = BigInt(2),
          c = BigInt(8),
          l = { zip215: !0 };
        e.twistedEdwards = function (t) {
          const e = (function (t) {
              const e = (0, s.validateBasic)(t);
              return (
                o.validateObject(
                  t,
                  {
                    hash: "function",
                    a: "bigint",
                    d: "bigint",
                    randomBytes: "function",
                  },
                  {
                    adjustScalarBytes: "function",
                    domain: "function",
                    uvRatio: "function",
                    mapToCurve: "function",
                  }
                ),
                Object.freeze({ ...e })
              );
            })(t),
            {
              Fp: r,
              n: h,
              prehash: d,
              hash: p,
              randomBytes: y,
              nByteLength: g,
              h: b,
            } = e,
            w = u << (BigInt(8 * g) - f),
            m = r.create,
            E =
              e.uvRatio ||
              ((t, e) => {
                try {
                  return { isValid: !0, value: r.sqrt(t * r.inv(e)) };
                } catch (t) {
                  return { isValid: !1, value: a };
                }
              }),
            B = e.adjustScalarBytes || ((t) => t),
            x =
              e.domain ||
              ((t, e, r) => {
                if (e.length || r)
                  throw new Error("Contexts/pre-hash are not supported");
                return t;
              }),
            v = (t) => "bigint" == typeof t && a < t,
            A = (t, e) => v(t) && v(e) && t < e,
            I = (t) => t === a || A(t, w);
          function S(t, e) {
            if (A(t, e)) return t;
            throw new Error(
              `Expected valid scalar < ${e}, got ${typeof t} ${t}`
            );
          }
          function U(t) {
            return t === a ? t : S(t, h);
          }
          const O = new Map();
          function T(t) {
            if (!(t instanceof k)) throw new Error("ExtendedPoint expected");
          }
          class k {
            constructor(t, e, r, n) {
              if (
                ((this.ex = t),
                (this.ey = e),
                (this.ez = r),
                (this.et = n),
                !I(t))
              )
                throw new Error("x required");
              if (!I(e)) throw new Error("y required");
              if (!I(r)) throw new Error("z required");
              if (!I(n)) throw new Error("t required");
            }
            get x() {
              return this.toAffine().x;
            }
            get y() {
              return this.toAffine().y;
            }
            static fromAffine(t) {
              if (t instanceof k) throw new Error("extended point not allowed");
              const { x: e, y: r } = t || {};
              if (!I(e) || !I(r)) throw new Error("invalid affine point");
              return new k(e, r, f, m(e * r));
            }
            static normalizeZ(t) {
              const e = r.invertBatch(t.map((t) => t.ez));
              return t.map((t, r) => t.toAffine(e[r])).map(k.fromAffine);
            }
            _setWindowSize(t) {
              (this._WINDOW_SIZE = t), O.delete(this);
            }
            assertValidity() {
              const { a: t, d: r } = e;
              if (this.is0()) throw new Error("bad point: ZERO");
              const { ex: n, ey: o, ez: i, et: s } = this,
                a = m(n * n),
                f = m(o * o),
                u = m(i * i),
                c = m(u * u),
                l = m(a * t);
              if (m(u * m(l + f)) !== m(c + m(r * m(a * f))))
                throw new Error("bad point: equation left != right (1)");
              if (m(n * o) !== m(i * s))
                throw new Error("bad point: equation left != right (2)");
            }
            equals(t) {
              T(t);
              const { ex: e, ey: r, ez: n } = this,
                { ex: o, ey: i, ez: s } = t,
                a = m(e * s),
                f = m(o * n),
                u = m(r * s),
                c = m(i * n);
              return a === f && u === c;
            }
            is0() {
              return this.equals(k.ZERO);
            }
            negate() {
              return new k(m(-this.ex), this.ey, this.ez, m(-this.et));
            }
            double() {
              const { a: t } = e,
                { ex: r, ey: n, ez: o } = this,
                i = m(r * r),
                s = m(n * n),
                a = m(u * m(o * o)),
                f = m(t * i),
                c = r + n,
                l = m(m(c * c) - i - s),
                h = f + s,
                d = h - a,
                p = f - s,
                y = m(l * d),
                g = m(h * p),
                b = m(l * p),
                w = m(d * h);
              return new k(y, g, w, b);
            }
            add(t) {
              T(t);
              const { a: r, d: n } = e,
                { ex: o, ey: i, ez: s, et: f } = this,
                { ex: c, ey: l, ez: h, et: d } = t;
              if (r === BigInt(-1)) {
                const t = m((i - o) * (l + c)),
                  e = m((i + o) * (l - c)),
                  r = m(e - t);
                if (r === a) return this.double();
                const n = m(s * u * d),
                  p = m(f * u * h),
                  y = p + n,
                  g = e + t,
                  b = p - n,
                  w = m(y * r),
                  E = m(g * b),
                  B = m(y * b),
                  x = m(r * g);
                return new k(w, E, x, B);
              }
              const p = m(o * c),
                y = m(i * l),
                g = m(f * n * d),
                b = m(s * h),
                w = m((o + i) * (c + l) - p - y),
                E = b - g,
                B = b + g,
                x = m(y - r * p),
                v = m(w * E),
                A = m(B * x),
                I = m(w * x),
                S = m(E * B);
              return new k(v, A, S, I);
            }
            subtract(t) {
              return this.add(t.negate());
            }
            wNAF(t) {
              return _.wNAFCached(this, O, t, k.normalizeZ);
            }
            multiply(t) {
              const { p: e, f: r } = this.wNAF(S(t, h));
              return k.normalizeZ([e, r])[0];
            }
            multiplyUnsafe(t) {
              let e = U(t);
              return e === a
                ? R
                : this.equals(R) || e === f
                  ? this
                  : this.equals(L)
                    ? this.wNAF(e).p
                    : _.unsafeLadder(this, e);
            }
            isSmallOrder() {
              return this.multiplyUnsafe(b).is0();
            }
            isTorsionFree() {
              return _.unsafeLadder(this, h).is0();
            }
            toAffine(t) {
              const { ex: e, ey: n, ez: o } = this,
                i = this.is0();
              null == t && (t = i ? c : r.inv(o));
              const s = m(e * t),
                u = m(n * t),
                l = m(o * t);
              if (i) return { x: a, y: f };
              if (l !== f) throw new Error("invZ was invalid");
              return { x: s, y: u };
            }
            clearCofactor() {
              const { h: t } = e;
              return t === f ? this : this.multiplyUnsafe(t);
            }
            static fromHex(t, n = !1) {
              const { d: s, a: u } = e,
                c = r.BYTES,
                l = (t = (0, i.ensureBytes)("pointHex", t, c)).slice(),
                h = t[c - 1];
              l[c - 1] = -129 & h;
              const d = o.bytesToNumberLE(l);
              d === a || S(d, n ? w : r.ORDER);
              const p = m(d * d),
                y = m(p - f),
                g = m(s * p - u);
              let { isValid: b, value: B } = E(y, g);
              if (!b) throw new Error("Point.fromHex: invalid y coordinate");
              const x = (B & f) === f,
                v = 0 != (128 & h);
              if (!n && B === a && v)
                throw new Error("Point.fromHex: x=0 and x_0=1");
              return v !== x && (B = m(-B)), k.fromAffine({ x: B, y: d });
            }
            static fromPrivateKey(t) {
              return j(t).point;
            }
            toRawBytes() {
              const { x: t, y: e } = this.toAffine(),
                n = o.numberToBytesLE(e, r.BYTES);
              return (n[n.length - 1] |= t & f ? 128 : 0), n;
            }
            toHex() {
              return o.bytesToHex(this.toRawBytes());
            }
          }
          (k.BASE = new k(e.Gx, e.Gy, f, m(e.Gx * e.Gy))),
            (k.ZERO = new k(a, f, f, a));
          const { BASE: L, ZERO: R } = k,
            _ = (0, s.wNAF)(k, 8 * g);
          function M(t) {
            return (0, n.mod)(t, h);
          }
          function C(t) {
            return M(o.bytesToNumberLE(t));
          }
          function j(t) {
            const e = g;
            t = (0, i.ensureBytes)("private key", t, e);
            const r = (0, i.ensureBytes)("hashed private key", p(t), 2 * e),
              n = B(r.slice(0, e)),
              o = r.slice(e, 2 * e),
              s = C(n),
              a = L.multiply(s),
              f = a.toRawBytes();
            return { head: n, prefix: o, scalar: s, point: a, pointBytes: f };
          }
          function N(t = new Uint8Array(), ...e) {
            const r = o.concatBytes(...e);
            return C(p(x(r, (0, i.ensureBytes)("context", t), !!d)));
          }
          const P = l;
          return (
            L._setWindowSize(8),
            {
              CURVE: e,
              getPublicKey: function (t) {
                return j(t).pointBytes;
              },
              sign: function (t, e, n = {}) {
                (t = (0, i.ensureBytes)("message", t)), d && (t = d(t));
                const { prefix: s, scalar: a, pointBytes: f } = j(e),
                  u = N(n.context, s, t),
                  c = L.multiply(u).toRawBytes(),
                  l = M(u + N(n.context, c, f, t) * a);
                U(l);
                const h = o.concatBytes(c, o.numberToBytesLE(l, r.BYTES));
                return (0, i.ensureBytes)("result", h, 2 * g);
              },
              verify: function (t, e, n, s = P) {
                const { context: a, zip215: f } = s,
                  u = r.BYTES;
                (t = (0, i.ensureBytes)("signature", t, 2 * u)),
                  (e = (0, i.ensureBytes)("message", e)),
                  d && (e = d(e));
                const c = o.bytesToNumberLE(t.slice(u, 2 * u));
                let l, h, p;
                try {
                  (l = k.fromHex(n, f)),
                    (h = k.fromHex(t.slice(0, u), f)),
                    (p = L.multiplyUnsafe(c));
                } catch (t) {
                  return !1;
                }
                if (!f && l.isSmallOrder()) return !1;
                const y = N(a, h.toRawBytes(), l.toRawBytes(), e);
                return h
                  .add(l.multiplyUnsafe(y))
                  .subtract(p)
                  .clearCofactor()
                  .equals(k.ZERO);
              },
              ExtendedPoint: k,
              utils: {
                getExtendedPublicKey: j,
                randomPrivateKey: () => y(r.BYTES),
                precompute: (t = 8, e = k.BASE) => (
                  e._setWindowSize(t), e.multiply(BigInt(3)), e
                ),
              },
            }
          );
        };
      },
      761: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.createHasher =
            e.isogenyMap =
            e.hash_to_field =
            e.expand_message_xof =
            e.expand_message_xmd =
              void 0);
        const n = r(967),
          o = r(484),
          i = o.bytesToNumberBE;
        function s(t, e) {
          if (t < 0 || t >= 1 << (8 * e))
            throw new Error(`bad I2OSP call: value=${t} length=${e}`);
          const r = Array.from({ length: e }).fill(0);
          for (let n = e - 1; n >= 0; n--) (r[n] = 255 & t), (t >>>= 8);
          return new Uint8Array(r);
        }
        function a(t, e) {
          const r = new Uint8Array(t.length);
          for (let n = 0; n < t.length; n++) r[n] = t[n] ^ e[n];
          return r;
        }
        function f(t) {
          if (!(0, o.isBytes)(t)) throw new Error("Uint8Array expected");
        }
        function u(t) {
          if (!Number.isSafeInteger(t)) throw new Error("number expected");
        }
        function c(t, e, r, n) {
          f(t),
            f(e),
            u(r),
            e.length > 255 &&
              (e = n(
                (0, o.concatBytes)((0, o.utf8ToBytes)("H2C-OVERSIZE-DST-"), e)
              ));
          const { outputLen: i, blockLen: c } = n,
            l = Math.ceil(r / i);
          if (l > 255) throw new Error("Invalid xmd length");
          const h = (0, o.concatBytes)(e, s(e.length, 1)),
            d = s(0, c),
            p = s(r, 2),
            y = new Array(l),
            g = n((0, o.concatBytes)(d, t, p, s(0, 1), h));
          y[0] = n((0, o.concatBytes)(g, s(1, 1), h));
          for (let t = 1; t <= l; t++) {
            const e = [a(g, y[t - 1]), s(t + 1, 1), h];
            y[t] = n((0, o.concatBytes)(...e));
          }
          return (0, o.concatBytes)(...y).slice(0, r);
        }
        function l(t, e, r, n, i) {
          if ((f(t), f(e), u(r), e.length > 255)) {
            const t = Math.ceil((2 * n) / 8);
            e = i
              .create({ dkLen: t })
              .update((0, o.utf8ToBytes)("H2C-OVERSIZE-DST-"))
              .update(e)
              .digest();
          }
          if (r > 65535 || e.length > 255)
            throw new Error("expand_message_xof: invalid lenInBytes");
          return i
            .create({ dkLen: r })
            .update(t)
            .update(s(r, 2))
            .update(e)
            .update(s(e.length, 1))
            .digest();
        }
        function h(t, e, r) {
          (0, o.validateObject)(r, {
            DST: "stringOrUint8Array",
            p: "bigint",
            m: "isSafeInteger",
            k: "isSafeInteger",
            hash: "hash",
          });
          const { p: s, k: a, m: h, hash: d, expand: p, DST: y } = r;
          f(t), u(e);
          const g = (function (t) {
              if ((0, o.isBytes)(t)) return t;
              if ("string" == typeof t) return (0, o.utf8ToBytes)(t);
              throw new Error("DST must be Uint8Array or string");
            })(y),
            b = s.toString(2).length,
            w = Math.ceil((b + a) / 8),
            m = e * h * w;
          let E;
          if ("xmd" === p) E = c(t, g, m, d);
          else if ("xof" === p) E = l(t, g, m, a, d);
          else {
            if ("_internal_pass" !== p)
              throw new Error('expand must be "xmd" or "xof"');
            E = t;
          }
          const B = new Array(e);
          for (let t = 0; t < e; t++) {
            const e = new Array(h);
            for (let r = 0; r < h; r++) {
              const o = w * (r + t * h),
                a = E.subarray(o, o + w);
              e[r] = (0, n.mod)(i(a), s);
            }
            B[t] = e;
          }
          return B;
        }
        (e.expand_message_xmd = c),
          (e.expand_message_xof = l),
          (e.hash_to_field = h),
          (e.isogenyMap = function (t, e) {
            const r = e.map((t) => Array.from(t).reverse());
            return (e, n) => {
              const [o, i, s, a] = r.map((r) =>
                r.reduce((r, n) => t.add(t.mul(r, e), n))
              );
              return (
                (e = t.div(o, i)), (n = t.mul(n, t.div(s, a))), { x: e, y: n }
              );
            };
          }),
          (e.createHasher = function (t, e, r) {
            if ("function" != typeof e)
              throw new Error("mapToCurve() must be defined");
            return {
              hashToCurve(n, o) {
                const i = h(n, 2, { ...r, DST: r.DST, ...o }),
                  s = t.fromAffine(e(i[0])),
                  a = t.fromAffine(e(i[1])),
                  f = s.add(a).clearCofactor();
                return f.assertValidity(), f;
              },
              encodeToCurve(n, o) {
                const i = h(n, 1, { ...r, DST: r.encodeDST, ...o }),
                  s = t.fromAffine(e(i[0])).clearCofactor();
                return s.assertValidity(), s;
              },
            };
          });
      },
      967: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.mapHashToField =
            e.getMinHashLength =
            e.getFieldBytesLength =
            e.hashToPrivateScalar =
            e.FpSqrtEven =
            e.FpSqrtOdd =
            e.Field =
            e.nLength =
            e.FpIsSquare =
            e.FpDiv =
            e.FpInvertBatch =
            e.FpPow =
            e.validateField =
            e.isNegativeLE =
            e.FpSqrt =
            e.tonelliShanks =
            e.invert =
            e.pow2 =
            e.pow =
            e.mod =
              void 0);
        const n = r(484),
          o = BigInt(0),
          i = BigInt(1),
          s = BigInt(2),
          a = BigInt(3),
          f = BigInt(4),
          u = BigInt(5),
          c = BigInt(8);
        function l(t, e) {
          const r = t % e;
          return r >= o ? r : e + r;
        }
        function h(t, e, r) {
          if (r <= o || e < o) throw new Error("Expected power/modulo > 0");
          if (r === i) return o;
          let n = i;
          for (; e > o; )
            e & i && (n = (n * t) % r), (t = (t * t) % r), (e >>= i);
          return n;
        }
        function d(t, e) {
          if (t === o || e <= o)
            throw new Error(
              `invert: expected positive integers, got n=${t} mod=${e}`
            );
          let r = l(t, e),
            n = e,
            s = o,
            a = i,
            f = i,
            u = o;
          for (; r !== o; ) {
            const t = n / r,
              e = n % r,
              o = s - f * t,
              i = a - u * t;
            (n = r), (r = e), (s = f), (a = u), (f = o), (u = i);
          }
          if (n !== i) throw new Error("invert: does not exist");
          return l(s, e);
        }
        function p(t) {
          const e = (t - i) / s;
          let r, n, a;
          for (r = t - i, n = 0; r % s === o; r /= s, n++);
          for (a = s; a < t && h(a, e, t) !== t - i; a++);
          if (1 === n) {
            const e = (t + i) / f;
            return function (t, r) {
              const n = t.pow(r, e);
              if (!t.eql(t.sqr(n), r))
                throw new Error("Cannot find square root");
              return n;
            };
          }
          const u = (r + i) / s;
          return function (t, o) {
            if (t.pow(o, e) === t.neg(t.ONE))
              throw new Error("Cannot find square root");
            let s = n,
              f = t.pow(t.mul(t.ONE, a), r),
              c = t.pow(o, u),
              l = t.pow(o, r);
            for (; !t.eql(l, t.ONE); ) {
              if (t.eql(l, t.ZERO)) return t.ZERO;
              let e = 1;
              for (let r = t.sqr(l); e < s && !t.eql(r, t.ONE); e++)
                r = t.sqr(r);
              const r = t.pow(f, i << BigInt(s - e - 1));
              (f = t.sqr(r)), (c = t.mul(c, r)), (l = t.mul(l, f)), (s = e);
            }
            return c;
          };
        }
        function y(t) {
          if (t % f === a) {
            const e = (t + i) / f;
            return function (t, r) {
              const n = t.pow(r, e);
              if (!t.eql(t.sqr(n), r))
                throw new Error("Cannot find square root");
              return n;
            };
          }
          if (t % c === u) {
            const e = (t - u) / c;
            return function (t, r) {
              const n = t.mul(r, s),
                o = t.pow(n, e),
                i = t.mul(r, o),
                a = t.mul(t.mul(i, s), o),
                f = t.mul(i, t.sub(a, t.ONE));
              if (!t.eql(t.sqr(f), r))
                throw new Error("Cannot find square root");
              return f;
            };
          }
          return p(t);
        }
        BigInt(9),
          BigInt(16),
          (e.mod = l),
          (e.pow = h),
          (e.pow2 = function (t, e, r) {
            let n = t;
            for (; e-- > o; ) (n *= n), (n %= r);
            return n;
          }),
          (e.invert = d),
          (e.tonelliShanks = p),
          (e.FpSqrt = y),
          (e.isNegativeLE = (t, e) => (l(t, e) & i) === i);
        const g = [
          "create",
          "isValid",
          "is0",
          "neg",
          "inv",
          "sqrt",
          "sqr",
          "eql",
          "add",
          "sub",
          "mul",
          "pow",
          "div",
          "addN",
          "subN",
          "mulN",
          "sqrN",
        ];
        function b(t, e, r) {
          if (r < o) throw new Error("Expected power > 0");
          if (r === o) return t.ONE;
          if (r === i) return e;
          let n = t.ONE,
            s = e;
          for (; r > o; ) r & i && (n = t.mul(n, s)), (s = t.sqr(s)), (r >>= i);
          return n;
        }
        function w(t, e) {
          const r = new Array(e.length),
            n = e.reduce(
              (e, n, o) => (t.is0(n) ? e : ((r[o] = e), t.mul(e, n))),
              t.ONE
            ),
            o = t.inv(n);
          return (
            e.reduceRight(
              (e, n, o) =>
                t.is0(n) ? e : ((r[o] = t.mul(e, r[o])), t.mul(e, n)),
              o
            ),
            r
          );
        }
        function m(t, e) {
          const r = void 0 !== e ? e : t.toString(2).length;
          return { nBitLength: r, nByteLength: Math.ceil(r / 8) };
        }
        function E(t) {
          if ("bigint" != typeof t)
            throw new Error("field order must be bigint");
          const e = t.toString(2).length;
          return Math.ceil(e / 8);
        }
        function B(t) {
          const e = E(t);
          return e + Math.ceil(e / 2);
        }
        (e.validateField = function (t) {
          const e = g.reduce((t, e) => ((t[e] = "function"), t), {
            ORDER: "bigint",
            MASK: "bigint",
            BYTES: "isSafeInteger",
            BITS: "isSafeInteger",
          });
          return (0, n.validateObject)(t, e);
        }),
          (e.FpPow = b),
          (e.FpInvertBatch = w),
          (e.FpDiv = function (t, e, r) {
            return t.mul(e, "bigint" == typeof r ? d(r, t.ORDER) : t.inv(r));
          }),
          (e.FpIsSquare = function (t) {
            const e = (t.ORDER - i) / s;
            return (r) => {
              const n = t.pow(r, e);
              return t.eql(n, t.ZERO) || t.eql(n, t.ONE);
            };
          }),
          (e.nLength = m),
          (e.Field = function (t, e, r = !1, s = {}) {
            if (t <= o) throw new Error(`Expected Field ORDER > 0, got ${t}`);
            const { nBitLength: a, nByteLength: f } = m(t, e);
            if (f > 2048)
              throw new Error(
                "Field lengths over 2048 bytes are not supported"
              );
            const u = y(t),
              c = Object.freeze({
                ORDER: t,
                BITS: a,
                BYTES: f,
                MASK: (0, n.bitMask)(a),
                ZERO: o,
                ONE: i,
                create: (e) => l(e, t),
                isValid: (e) => {
                  if ("bigint" != typeof e)
                    throw new Error(
                      "Invalid field element: expected bigint, got " + typeof e
                    );
                  return o <= e && e < t;
                },
                is0: (t) => t === o,
                isOdd: (t) => (t & i) === i,
                neg: (e) => l(-e, t),
                eql: (t, e) => t === e,
                sqr: (e) => l(e * e, t),
                add: (e, r) => l(e + r, t),
                sub: (e, r) => l(e - r, t),
                mul: (e, r) => l(e * r, t),
                pow: (t, e) => b(c, t, e),
                div: (e, r) => l(e * d(r, t), t),
                sqrN: (t) => t * t,
                addN: (t, e) => t + e,
                subN: (t, e) => t - e,
                mulN: (t, e) => t * e,
                inv: (e) => d(e, t),
                sqrt: s.sqrt || ((t) => u(c, t)),
                invertBatch: (t) => w(c, t),
                cmov: (t, e, r) => (r ? e : t),
                toBytes: (t) =>
                  r
                    ? (0, n.numberToBytesLE)(t, f)
                    : (0, n.numberToBytesBE)(t, f),
                fromBytes: (t) => {
                  if (t.length !== f)
                    throw new Error(
                      `Fp.fromBytes: expected ${f}, got ${t.length}`
                    );
                  return r
                    ? (0, n.bytesToNumberLE)(t)
                    : (0, n.bytesToNumberBE)(t);
                },
              });
            return Object.freeze(c);
          }),
          (e.FpSqrtOdd = function (t, e) {
            if (!t.isOdd) throw new Error("Field doesn't have isOdd");
            const r = t.sqrt(e);
            return t.isOdd(r) ? r : t.neg(r);
          }),
          (e.FpSqrtEven = function (t, e) {
            if (!t.isOdd) throw new Error("Field doesn't have isOdd");
            const r = t.sqrt(e);
            return t.isOdd(r) ? t.neg(r) : r;
          }),
          (e.hashToPrivateScalar = function (t, e, r = !1) {
            const o = (t = (0, n.ensureBytes)("privateHash", t)).length,
              s = m(e).nByteLength + 8;
            if (s < 24 || o < s || o > 1024)
              throw new Error(
                `hashToPrivateScalar: expected ${s}-1024 bytes of input, got ${o}`
              );
            return (
              l(
                r ? (0, n.bytesToNumberLE)(t) : (0, n.bytesToNumberBE)(t),
                e - i
              ) + i
            );
          }),
          (e.getFieldBytesLength = E),
          (e.getMinHashLength = B),
          (e.mapHashToField = function (t, e, r = !1) {
            const o = t.length,
              s = E(e),
              a = B(e);
            if (o < 16 || o < a || o > 1024)
              throw new Error(`expected ${a}-1024 bytes of input, got ${o}`);
            const f =
              l(
                r ? (0, n.bytesToNumberBE)(t) : (0, n.bytesToNumberLE)(t),
                e - i
              ) + i;
            return r
              ? (0, n.numberToBytesLE)(f, s)
              : (0, n.numberToBytesBE)(f, s);
          });
      },
      854: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.montgomery = void 0);
        const n = r(967),
          o = r(484),
          i = BigInt(0),
          s = BigInt(1);
        e.montgomery = function (t) {
          const e =
            ((r = t),
            (0, o.validateObject)(
              r,
              { a: "bigint" },
              {
                montgomeryBits: "isSafeInteger",
                nByteLength: "isSafeInteger",
                adjustScalarBytes: "function",
                domain: "function",
                powPminus2: "function",
                Gu: "bigint",
              }
            ),
            Object.freeze({ ...r }));
          var r;
          const { P: a } = e,
            f = (t) => (0, n.mod)(t, a),
            u = e.montgomeryBits,
            c = Math.ceil(u / 8),
            l = e.nByteLength,
            h = e.adjustScalarBytes || ((t) => t),
            d = e.powPminus2 || ((t) => (0, n.pow)(t, a - BigInt(2), a));
          function p(t, e, r) {
            const n = f(t * (e - r));
            return [(e = f(e - n)), (r = f(r + n))];
          }
          function y(t) {
            if ("bigint" == typeof t && i <= t && t < a) return t;
            throw new Error("Expected valid scalar 0 < scalar < CURVE.P");
          }
          const g = (e.a - BigInt(2)) / BigInt(4);
          function b(t) {
            return (0, o.numberToBytesLE)(f(t), c);
          }
          function w(t, e) {
            const r = (function (t) {
                const e = (0, o.ensureBytes)("u coordinate", t, c);
                return 32 === l && (e[31] &= 127), (0, o.bytesToNumberLE)(e);
              })(e),
              n = (function (t, e) {
                const r = y(t),
                  n = y(e),
                  o = r;
                let a,
                  c = s,
                  l = i,
                  h = r,
                  b = s,
                  w = i;
                for (let t = BigInt(u - 1); t >= i; t--) {
                  const e = (n >> t) & s;
                  (w ^= e),
                    (a = p(w, c, h)),
                    (c = a[0]),
                    (h = a[1]),
                    (a = p(w, l, b)),
                    (l = a[0]),
                    (b = a[1]),
                    (w = e);
                  const r = c + l,
                    i = f(r * r),
                    u = c - l,
                    d = f(u * u),
                    y = i - d,
                    m = h + b,
                    E = f((h - b) * r),
                    B = f(m * u),
                    x = E + B,
                    v = E - B;
                  (h = f(x * x)),
                    (b = f(o * f(v * v))),
                    (c = f(i * d)),
                    (l = f(y * (i + f(g * y))));
                }
                (a = p(w, c, h)),
                  (c = a[0]),
                  (h = a[1]),
                  (a = p(w, l, b)),
                  (l = a[0]),
                  (b = a[1]);
                const m = d(l);
                return f(c * m);
              })(
                r,
                (function (t) {
                  const e = (0, o.ensureBytes)("scalar", t),
                    r = e.length;
                  if (r !== c && r !== l)
                    throw new Error(`Expected ${c} or ${l} bytes, got ${r}`);
                  return (0, o.bytesToNumberLE)(h(e));
                })(t)
              );
            if (n === i)
              throw new Error("Invalid private or public key received");
            return b(n);
          }
          const m = b(e.Gu);
          function E(t) {
            return w(t, m);
          }
          return {
            scalarMult: w,
            scalarMultBase: E,
            getSharedSecret: (t, e) => w(t, e),
            getPublicKey: (t) => E(t),
            utils: { randomPrivateKey: () => e.randomBytes(e.nByteLength) },
            GuBytes: m,
          };
        };
      },
      484: (t, e) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.validateObject =
            e.createHmacDrbg =
            e.bitMask =
            e.bitSet =
            e.bitGet =
            e.bitLen =
            e.utf8ToBytes =
            e.equalBytes =
            e.concatBytes =
            e.ensureBytes =
            e.numberToVarBytesBE =
            e.numberToBytesLE =
            e.numberToBytesBE =
            e.bytesToNumberLE =
            e.bytesToNumberBE =
            e.hexToBytes =
            e.hexToNumber =
            e.numberToHexUnpadded =
            e.bytesToHex =
            e.isBytes =
              void 0);
        const r = BigInt(0),
          n = BigInt(1),
          o = BigInt(2);
        function i(t) {
          return (
            t instanceof Uint8Array ||
            (null != t &&
              "object" == typeof t &&
              "Uint8Array" === t.constructor.name)
          );
        }
        e.isBytes = i;
        const s = Array.from({ length: 256 }, (t, e) =>
          e.toString(16).padStart(2, "0")
        );
        function a(t) {
          if (!i(t)) throw new Error("Uint8Array expected");
          let e = "";
          for (let r = 0; r < t.length; r++) e += s[t[r]];
          return e;
        }
        function f(t) {
          const e = t.toString(16);
          return 1 & e.length ? `0${e}` : e;
        }
        function u(t) {
          if ("string" != typeof t)
            throw new Error("hex string expected, got " + typeof t);
          return BigInt("" === t ? "0" : `0x${t}`);
        }
        (e.bytesToHex = a), (e.numberToHexUnpadded = f), (e.hexToNumber = u);
        const c = { _0: 48, _9: 57, _A: 65, _F: 70, _a: 97, _f: 102 };
        function l(t) {
          return t >= c._0 && t <= c._9
            ? t - c._0
            : t >= c._A && t <= c._F
              ? t - (c._A - 10)
              : t >= c._a && t <= c._f
                ? t - (c._a - 10)
                : void 0;
        }
        function h(t) {
          if ("string" != typeof t)
            throw new Error("hex string expected, got " + typeof t);
          const e = t.length,
            r = e / 2;
          if (e % 2)
            throw new Error(
              "padded hex string expected, got unpadded hex of length " + e
            );
          const n = new Uint8Array(r);
          for (let e = 0, o = 0; e < r; e++, o += 2) {
            const r = l(t.charCodeAt(o)),
              i = l(t.charCodeAt(o + 1));
            if (void 0 === r || void 0 === i) {
              const e = t[o] + t[o + 1];
              throw new Error(
                'hex string expected, got non-hex character "' +
                  e +
                  '" at index ' +
                  o
              );
            }
            n[e] = 16 * r + i;
          }
          return n;
        }
        function d(t, e) {
          return h(t.toString(16).padStart(2 * e, "0"));
        }
        function p(...t) {
          let e = 0;
          for (let r = 0; r < t.length; r++) {
            const n = t[r];
            if (!i(n)) throw new Error("Uint8Array expected");
            e += n.length;
          }
          let r = new Uint8Array(e),
            n = 0;
          for (let e = 0; e < t.length; e++) {
            const o = t[e];
            r.set(o, n), (n += o.length);
          }
          return r;
        }
        (e.hexToBytes = h),
          (e.bytesToNumberBE = function (t) {
            return u(a(t));
          }),
          (e.bytesToNumberLE = function (t) {
            if (!i(t)) throw new Error("Uint8Array expected");
            return u(a(Uint8Array.from(t).reverse()));
          }),
          (e.numberToBytesBE = d),
          (e.numberToBytesLE = function (t, e) {
            return d(t, e).reverse();
          }),
          (e.numberToVarBytesBE = function (t) {
            return h(f(t));
          }),
          (e.ensureBytes = function (t, e, r) {
            let n;
            if ("string" == typeof e)
              try {
                n = h(e);
              } catch (r) {
                throw new Error(
                  `${t} must be valid hex string, got "${e}". Cause: ${r}`
                );
              }
            else {
              if (!i(e))
                throw new Error(`${t} must be hex string or Uint8Array`);
              n = Uint8Array.from(e);
            }
            const o = n.length;
            if ("number" == typeof r && o !== r)
              throw new Error(`${t} expected ${r} bytes, got ${o}`);
            return n;
          }),
          (e.concatBytes = p),
          (e.equalBytes = function (t, e) {
            if (t.length !== e.length) return !1;
            let r = 0;
            for (let n = 0; n < t.length; n++) r |= t[n] ^ e[n];
            return 0 === r;
          }),
          (e.utf8ToBytes = function (t) {
            if ("string" != typeof t)
              throw new Error("utf8ToBytes expected string, got " + typeof t);
            return new Uint8Array(new TextEncoder().encode(t));
          }),
          (e.bitLen = function (t) {
            let e;
            for (e = 0; t > r; t >>= n, e += 1);
            return e;
          }),
          (e.bitGet = function (t, e) {
            return (t >> BigInt(e)) & n;
          }),
          (e.bitSet = (t, e, o) => t | ((o ? n : r) << BigInt(e))),
          (e.bitMask = (t) => (o << BigInt(t - 1)) - n);
        const y = (t) => new Uint8Array(t),
          g = (t) => Uint8Array.from(t);
        e.createHmacDrbg = function (t, e, r) {
          if ("number" != typeof t || t < 2)
            throw new Error("hashLen must be a number");
          if ("number" != typeof e || e < 2)
            throw new Error("qByteLen must be a number");
          if ("function" != typeof r)
            throw new Error("hmacFn must be a function");
          let n = y(t),
            o = y(t),
            i = 0;
          const s = () => {
              n.fill(1), o.fill(0), (i = 0);
            },
            a = (...t) => r(o, n, ...t),
            f = (t = y()) => {
              (o = a(g([0]), t)),
                (n = a()),
                0 !== t.length && ((o = a(g([1]), t)), (n = a()));
            },
            u = () => {
              if (i++ >= 1e3) throw new Error("drbg: tried 1000 values");
              let t = 0;
              const r = [];
              for (; t < e; ) {
                n = a();
                const e = n.slice();
                r.push(e), (t += n.length);
              }
              return p(...r);
            };
          return (t, e) => {
            let r;
            for (s(), f(t); !(r = e(u())); ) f();
            return s(), r;
          };
        };
        const b = {
          bigint: (t) => "bigint" == typeof t,
          function: (t) => "function" == typeof t,
          boolean: (t) => "boolean" == typeof t,
          string: (t) => "string" == typeof t,
          stringOrUint8Array: (t) => "string" == typeof t || i(t),
          isSafeInteger: (t) => Number.isSafeInteger(t),
          array: (t) => Array.isArray(t),
          field: (t, e) => e.Fp.isValid(t),
          hash: (t) =>
            "function" == typeof t && Number.isSafeInteger(t.outputLen),
        };
        e.validateObject = function (t, e, r = {}) {
          const n = (e, r, n) => {
            const o = b[r];
            if ("function" != typeof o)
              throw new Error(`Invalid validator "${r}", expected function`);
            const i = t[e];
            if (!((n && void 0 === i) || o(i, t)))
              throw new Error(
                `Invalid param ${String(e)}=${i} (${typeof i}), expected ${r}`
              );
          };
          for (const [t, r] of Object.entries(e)) n(t, r, !1);
          for (const [t, e] of Object.entries(r)) n(t, e, !0);
          return t;
        };
      },
      459: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.hash_to_ristretto255 =
            e.hashToRistretto255 =
            e.RistrettoPoint =
            e.encodeToCurve =
            e.hashToCurve =
            e.edwardsToMontgomeryPriv =
            e.edwardsToMontgomery =
            e.edwardsToMontgomeryPub =
            e.x25519 =
            e.ed25519ph =
            e.ed25519ctx =
            e.ed25519 =
            e.ED25519_TORSION_SUBGROUP =
              void 0);
        const n = r(102),
          o = r(175),
          i = r(377),
          s = r(854),
          a = r(967),
          f = r(484),
          u = r(761),
          c = BigInt(
            "57896044618658097711785492504343953926634992332820282019728792003956564819949"
          ),
          l = BigInt(
            "19681161376707505956807079304988542015446066515923890162744021073123829784752"
          ),
          h = BigInt(0),
          d = BigInt(1),
          p = BigInt(2),
          y = BigInt(5),
          g = BigInt(10),
          b = BigInt(20),
          w = BigInt(40),
          m = BigInt(80);
        function E(t) {
          const e = c,
            r = (((t * t) % e) * t) % e,
            n = ((0, a.pow2)(r, p, e) * r) % e,
            o = ((0, a.pow2)(n, d, e) * t) % e,
            i = ((0, a.pow2)(o, y, e) * o) % e,
            s = ((0, a.pow2)(i, g, e) * i) % e,
            f = ((0, a.pow2)(s, b, e) * s) % e,
            u = ((0, a.pow2)(f, w, e) * f) % e,
            l = ((0, a.pow2)(u, m, e) * u) % e,
            h = ((0, a.pow2)(l, m, e) * u) % e,
            E = ((0, a.pow2)(h, g, e) * i) % e;
          return { pow_p_5_8: ((0, a.pow2)(E, p, e) * t) % e, b2: r };
        }
        function B(t) {
          return (t[0] &= 248), (t[31] &= 127), (t[31] |= 64), t;
        }
        function x(t, e) {
          const r = c,
            n = (0, a.mod)(e * e * e, r),
            o = E(t * (0, a.mod)(n * n * e, r)).pow_p_5_8;
          let i = (0, a.mod)(t * n * o, r);
          const s = (0, a.mod)(e * i * i, r),
            f = i,
            u = (0, a.mod)(i * l, r),
            h = s === t,
            d = s === (0, a.mod)(-t, r),
            p = s === (0, a.mod)(-t * l, r);
          return (
            h && (i = f),
            (d || p) && (i = u),
            (0, a.isNegativeLE)(i, r) && (i = (0, a.mod)(-i, r)),
            { isValid: h || d, value: i }
          );
        }
        e.ED25519_TORSION_SUBGROUP = [
          "0100000000000000000000000000000000000000000000000000000000000000",
          "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
          "0000000000000000000000000000000000000000000000000000000000000080",
          "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
          "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
          "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
          "0000000000000000000000000000000000000000000000000000000000000000",
          "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
        ];
        const v = (0, a.Field)(c, void 0, !0),
          A = {
            a: BigInt(-1),
            d: BigInt(
              "37095705934669439343138083508754565189542113879843219016388785533085940283555"
            ),
            Fp: v,
            n: BigInt(
              "7237005577332262213973186563042994240857116359379907606001950938285454250989"
            ),
            h: BigInt(8),
            Gx: BigInt(
              "15112221349535400772501151409588531511454012693041857206046113283949847762202"
            ),
            Gy: BigInt(
              "46316835694926478169428394003475163141307993866256225615783033603165251855960"
            ),
            hash: n.sha512,
            randomBytes: o.randomBytes,
            adjustScalarBytes: B,
            uvRatio: x,
          };
        function I(t, e, r) {
          if (e.length > 255) throw new Error("Context is too big");
          return (0, o.concatBytes)(
            (0, o.utf8ToBytes)("SigEd25519 no Ed25519 collisions"),
            new Uint8Array([r ? 1 : 0, e.length]),
            e,
            t
          );
        }
        function S(t) {
          const { y: r } = e.ed25519.ExtendedPoint.fromHex(t),
            n = BigInt(1);
          return v.toBytes(v.create((n + r) * v.inv(n - r)));
        }
        (e.ed25519 = (0, i.twistedEdwards)(A)),
          (e.ed25519ctx = (0, i.twistedEdwards)({ ...A, domain: I })),
          (e.ed25519ph = (0, i.twistedEdwards)({
            ...A,
            domain: I,
            prehash: n.sha512,
          })),
          (e.x25519 = (0, s.montgomery)({
            P: c,
            a: BigInt(486662),
            montgomeryBits: 255,
            nByteLength: 32,
            Gu: BigInt(9),
            powPminus2: (t) => {
              const e = c,
                { pow_p_5_8: r, b2: n } = E(t);
              return (0, a.mod)((0, a.pow2)(r, BigInt(3), e) * n, e);
            },
            adjustScalarBytes: B,
            randomBytes: o.randomBytes,
          })),
          (e.edwardsToMontgomeryPub = S),
          (e.edwardsToMontgomery = S),
          (e.edwardsToMontgomeryPriv = function (t) {
            const e = A.hash(t.subarray(0, 32));
            return A.adjustScalarBytes(e).subarray(0, 32);
          });
        const U = (v.ORDER + BigInt(3)) / BigInt(8),
          O = v.pow(p, U),
          T = v.sqrt(v.neg(v.ONE)),
          k = (v.ORDER - BigInt(5)) / BigInt(8),
          L = BigInt(486662),
          R = (0, a.FpSqrtEven)(v, v.neg(BigInt(486664)));
        const _ = (() =>
          (0, u.createHasher)(
            e.ed25519.ExtendedPoint,
            (t) =>
              (function (t) {
                const {
                  xMn: e,
                  xMd: r,
                  yMn: n,
                  yMd: o,
                } = (function (t) {
                  let e = v.sqr(t);
                  e = v.mul(e, p);
                  let r = v.add(e, v.ONE),
                    n = v.neg(L),
                    o = v.sqr(r),
                    i = v.mul(o, r),
                    s = v.mul(e, L);
                  (s = v.mul(s, n)), (s = v.add(s, o)), (s = v.mul(s, n));
                  let a = v.sqr(i);
                  (o = v.sqr(a)),
                    (a = v.mul(a, i)),
                    (a = v.mul(a, s)),
                    (o = v.mul(o, a));
                  let f = v.pow(o, k);
                  f = v.mul(f, a);
                  let u = v.mul(f, T);
                  (o = v.sqr(f)), (o = v.mul(o, i));
                  let c = v.eql(o, s),
                    l = v.cmov(u, f, c),
                    h = v.mul(n, e),
                    y = v.mul(f, t);
                  y = v.mul(y, O);
                  let g = v.mul(y, T),
                    b = v.mul(s, e);
                  (o = v.sqr(y)), (o = v.mul(o, i));
                  let w = v.eql(o, b),
                    m = v.cmov(g, y, w);
                  (o = v.sqr(l)), (o = v.mul(o, i));
                  let E = v.eql(o, s),
                    B = v.cmov(h, n, E),
                    x = v.cmov(m, l, E),
                    A = v.isOdd(x);
                  return (
                    (x = v.cmov(x, v.neg(x), E !== A)),
                    { xMn: B, xMd: r, yMn: x, yMd: d }
                  );
                })(t);
                let i = v.mul(e, o);
                i = v.mul(i, R);
                let s = v.mul(r, n),
                  a = v.sub(e, r),
                  f = v.add(e, r),
                  u = v.mul(s, f),
                  c = v.eql(u, v.ZERO);
                (i = v.cmov(i, v.ZERO, c)),
                  (s = v.cmov(s, v.ONE, c)),
                  (a = v.cmov(a, v.ONE, c)),
                  (f = v.cmov(f, v.ONE, c));
                const l = v.invertBatch([s, f]);
                return { x: v.mul(i, l[0]), y: v.mul(a, l[1]) };
              })(t[0]),
            {
              DST: "edwards25519_XMD:SHA-512_ELL2_RO_",
              encodeDST: "edwards25519_XMD:SHA-512_ELL2_NU_",
              p: v.ORDER,
              m: 1,
              k: 128,
              expand: "xmd",
              hash: n.sha512,
            }
          ))();
        function M(t) {
          if (!(t instanceof $)) throw new Error("RistrettoPoint expected");
        }
        (e.hashToCurve = _.hashToCurve), (e.encodeToCurve = _.encodeToCurve);
        const C = l,
          j = BigInt(
            "25063068953384623474111414158702152701244531502492656460079210482610430750235"
          ),
          N = BigInt(
            "54469307008909316920995813868745141605393597292927456921205312896311721017578"
          ),
          P = BigInt(
            "1159843021668779879193775521855586647937357759715417654439879720876111806838"
          ),
          F = BigInt(
            "40440834346308536858101042469323190826248399146238708352240133220865137265952"
          ),
          H = (t) => x(d, t),
          D = BigInt(
            "0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
          ),
          V = (t) => e.ed25519.CURVE.Fp.create((0, f.bytesToNumberLE)(t) & D);
        function q(t) {
          const { d: r } = e.ed25519.CURVE,
            n = e.ed25519.CURVE.Fp.ORDER,
            o = e.ed25519.CURVE.Fp.create,
            i = o(C * t * t),
            s = o((i + d) * P);
          let f = BigInt(-1);
          const u = o((f - r * i) * o(i + r));
          let { isValid: c, value: l } = x(s, u),
            h = o(l * t);
          (0, a.isNegativeLE)(h, n) || (h = o(-h)), c || (l = h), c || (f = i);
          const p = o(f * (i - d) * F - u),
            y = l * l,
            g = o((l + l) * u),
            b = o(p * j),
            w = o(d - y),
            m = o(d + y);
          return new e.ed25519.ExtendedPoint(
            o(g * m),
            o(w * b),
            o(b * m),
            o(g * w)
          );
        }
        class $ {
          constructor(t) {
            this.ep = t;
          }
          static fromAffine(t) {
            return new $(e.ed25519.ExtendedPoint.fromAffine(t));
          }
          static hashToCurve(t) {
            t = (0, f.ensureBytes)("ristrettoHash", t, 64);
            const e = q(V(t.slice(0, 32))),
              r = q(V(t.slice(32, 64)));
            return new $(e.add(r));
          }
          static fromHex(t) {
            t = (0, f.ensureBytes)("ristrettoHex", t, 32);
            const { a: r, d: n } = e.ed25519.CURVE,
              o = e.ed25519.CURVE.Fp.ORDER,
              i = e.ed25519.CURVE.Fp.create,
              s =
                "RistrettoPoint.fromHex: the hex is not valid encoding of RistrettoPoint",
              u = V(t);
            if (
              !(0, f.equalBytes)((0, f.numberToBytesLE)(u, 32), t) ||
              (0, a.isNegativeLE)(u, o)
            )
              throw new Error(s);
            const c = i(u * u),
              l = i(d + r * c),
              p = i(d - r * c),
              y = i(l * l),
              g = i(p * p),
              b = i(r * n * y - g),
              { isValid: w, value: m } = H(i(b * g)),
              E = i(m * p),
              B = i(m * E * b);
            let x = i((u + u) * E);
            (0, a.isNegativeLE)(x, o) && (x = i(-x));
            const v = i(l * B),
              A = i(x * v);
            if (!w || (0, a.isNegativeLE)(A, o) || v === h) throw new Error(s);
            return new $(new e.ed25519.ExtendedPoint(x, v, d, A));
          }
          toRawBytes() {
            let { ex: t, ey: r, ez: n, et: o } = this.ep;
            const i = e.ed25519.CURVE.Fp.ORDER,
              s = e.ed25519.CURVE.Fp.create,
              u = s(s(n + r) * s(n - r)),
              c = s(t * r),
              l = s(c * c),
              { value: h } = H(s(u * l)),
              d = s(h * u),
              p = s(h * c),
              y = s(d * p * o);
            let g;
            if ((0, a.isNegativeLE)(o * y, i)) {
              let e = s(r * C),
                n = s(t * C);
              (t = e), (r = n), (g = s(d * N));
            } else g = p;
            (0, a.isNegativeLE)(t * y, i) && (r = s(-r));
            let b = s((n - r) * g);
            return (
              (0, a.isNegativeLE)(b, i) && (b = s(-b)),
              (0, f.numberToBytesLE)(b, 32)
            );
          }
          toHex() {
            return (0, f.bytesToHex)(this.toRawBytes());
          }
          toString() {
            return this.toHex();
          }
          equals(t) {
            M(t);
            const { ex: r, ey: n } = this.ep,
              { ex: o, ey: i } = t.ep,
              s = e.ed25519.CURVE.Fp.create,
              a = s(r * i) === s(n * o),
              f = s(n * i) === s(r * o);
            return a || f;
          }
          add(t) {
            return M(t), new $(this.ep.add(t.ep));
          }
          subtract(t) {
            return M(t), new $(this.ep.subtract(t.ep));
          }
          multiply(t) {
            return new $(this.ep.multiply(t));
          }
          multiplyUnsafe(t) {
            return new $(this.ep.multiplyUnsafe(t));
          }
          double() {
            return new $(this.ep.double());
          }
          negate() {
            return new $(this.ep.negate());
          }
        }
        (e.RistrettoPoint =
          ($.BASE || ($.BASE = new $(e.ed25519.ExtendedPoint.BASE)),
          $.ZERO || ($.ZERO = new $(e.ed25519.ExtendedPoint.ZERO)),
          $)),
          (e.hashToRistretto255 = (t, e) => {
            const r = e.DST,
              i = "string" == typeof r ? (0, o.utf8ToBytes)(r) : r,
              s = (0, u.expand_message_xmd)(t, i, 64, n.sha512);
            return $.hashToCurve(s);
          }),
          (e.hash_to_ristretto255 = e.hashToRistretto255);
      },
      557: (t, e) => {
        "use strict";
        function r(t) {
          if (!Number.isSafeInteger(t) || t < 0)
            throw new Error(`Wrong positive integer: ${t}`);
        }
        function n(t) {
          if ("boolean" != typeof t)
            throw new Error(`Expected boolean, not ${t}`);
        }
        function o(t, ...e) {
          if (
            !(
              (r = t) instanceof Uint8Array ||
              (null != r &&
                "object" == typeof r &&
                "Uint8Array" === r.constructor.name)
            )
          )
            throw new Error("Expected Uint8Array");
          var r;
          if (e.length > 0 && !e.includes(t.length))
            throw new Error(
              `Expected Uint8Array of length ${e}, not of length=${t.length}`
            );
        }
        function i(t) {
          if ("function" != typeof t || "function" != typeof t.create)
            throw new Error("Hash should be wrapped by utils.wrapConstructor");
          r(t.outputLen), r(t.blockLen);
        }
        function s(t, e = !0) {
          if (t.destroyed) throw new Error("Hash instance has been destroyed");
          if (e && t.finished)
            throw new Error("Hash#digest() has already been called");
        }
        function a(t, e) {
          o(t);
          const r = e.outputLen;
          if (t.length < r)
            throw new Error(
              `digestInto() expects output buffer of length at least ${r}`
            );
        }
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.output = e.exists = e.hash = e.bytes = e.bool = e.number = void 0),
          (e.number = r),
          (e.bool = n),
          (e.bytes = o),
          (e.hash = i),
          (e.exists = s),
          (e.output = a);
        const f = {
          number: r,
          bool: n,
          bytes: o,
          hash: i,
          exists: s,
          output: a,
        };
        e.default = f;
      },
      915: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.SHA2 = void 0);
        const n = r(557),
          o = r(175);
        class i extends o.Hash {
          constructor(t, e, r, n) {
            super(),
              (this.blockLen = t),
              (this.outputLen = e),
              (this.padOffset = r),
              (this.isLE = n),
              (this.finished = !1),
              (this.length = 0),
              (this.pos = 0),
              (this.destroyed = !1),
              (this.buffer = new Uint8Array(t)),
              (this.view = (0, o.createView)(this.buffer));
          }
          update(t) {
            (0, n.exists)(this);
            const { view: e, buffer: r, blockLen: i } = this,
              s = (t = (0, o.toBytes)(t)).length;
            for (let n = 0; n < s; ) {
              const a = Math.min(i - this.pos, s - n);
              if (a !== i)
                r.set(t.subarray(n, n + a), this.pos),
                  (this.pos += a),
                  (n += a),
                  this.pos === i && (this.process(e, 0), (this.pos = 0));
              else {
                const e = (0, o.createView)(t);
                for (; i <= s - n; n += i) this.process(e, n);
              }
            }
            return (this.length += t.length), this.roundClean(), this;
          }
          digestInto(t) {
            (0, n.exists)(this), (0, n.output)(t, this), (this.finished = !0);
            const { buffer: e, view: r, blockLen: i, isLE: s } = this;
            let { pos: a } = this;
            (e[a++] = 128),
              this.buffer.subarray(a).fill(0),
              this.padOffset > i - a && (this.process(r, 0), (a = 0));
            for (let t = a; t < i; t++) e[t] = 0;
            !(function (t, e, r, n) {
              if ("function" == typeof t.setBigUint64)
                return t.setBigUint64(e, r, n);
              const o = BigInt(32),
                i = BigInt(4294967295),
                s = Number((r >> o) & i),
                a = Number(r & i),
                f = n ? 4 : 0,
                u = n ? 0 : 4;
              t.setUint32(e + f, s, n), t.setUint32(e + u, a, n);
            })(r, i - 8, BigInt(8 * this.length), s),
              this.process(r, 0);
            const f = (0, o.createView)(t),
              u = this.outputLen;
            if (u % 4)
              throw new Error("_sha2: outputLen should be aligned to 32bit");
            const c = u / 4,
              l = this.get();
            if (c > l.length)
              throw new Error("_sha2: outputLen bigger than state");
            for (let t = 0; t < c; t++) f.setUint32(4 * t, l[t], s);
          }
          digest() {
            const { buffer: t, outputLen: e } = this;
            this.digestInto(t);
            const r = t.slice(0, e);
            return this.destroy(), r;
          }
          _cloneInto(t) {
            t || (t = new this.constructor()), t.set(...this.get());
            const {
              blockLen: e,
              buffer: r,
              length: n,
              finished: o,
              destroyed: i,
              pos: s,
            } = this;
            return (
              (t.length = n),
              (t.pos = s),
              (t.finished = o),
              (t.destroyed = i),
              n % e && t.buffer.set(r),
              t
            );
          }
        }
        e.SHA2 = i;
      },
      318: (t, e) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.add5L =
            e.add5H =
            e.add4H =
            e.add4L =
            e.add3H =
            e.add3L =
            e.add =
            e.rotlBL =
            e.rotlBH =
            e.rotlSL =
            e.rotlSH =
            e.rotr32L =
            e.rotr32H =
            e.rotrBL =
            e.rotrBH =
            e.rotrSL =
            e.rotrSH =
            e.shrSL =
            e.shrSH =
            e.toBig =
            e.split =
            e.fromBig =
              void 0);
        const r = BigInt(2 ** 32 - 1),
          n = BigInt(32);
        function o(t, e = !1) {
          return e
            ? { h: Number(t & r), l: Number((t >> n) & r) }
            : { h: 0 | Number((t >> n) & r), l: 0 | Number(t & r) };
        }
        function i(t, e = !1) {
          let r = new Uint32Array(t.length),
            n = new Uint32Array(t.length);
          for (let i = 0; i < t.length; i++) {
            const { h: s, l: a } = o(t[i], e);
            [r[i], n[i]] = [s, a];
          }
          return [r, n];
        }
        (e.fromBig = o), (e.split = i);
        const s = (t, e) => (BigInt(t >>> 0) << n) | BigInt(e >>> 0);
        e.toBig = s;
        const a = (t, e, r) => t >>> r;
        e.shrSH = a;
        const f = (t, e, r) => (t << (32 - r)) | (e >>> r);
        e.shrSL = f;
        const u = (t, e, r) => (t >>> r) | (e << (32 - r));
        e.rotrSH = u;
        const c = (t, e, r) => (t << (32 - r)) | (e >>> r);
        e.rotrSL = c;
        const l = (t, e, r) => (t << (64 - r)) | (e >>> (r - 32));
        e.rotrBH = l;
        const h = (t, e, r) => (t >>> (r - 32)) | (e << (64 - r));
        e.rotrBL = h;
        const d = (t, e) => e;
        e.rotr32H = d;
        const p = (t, e) => t;
        e.rotr32L = p;
        const y = (t, e, r) => (t << r) | (e >>> (32 - r));
        e.rotlSH = y;
        const g = (t, e, r) => (e << r) | (t >>> (32 - r));
        e.rotlSL = g;
        const b = (t, e, r) => (e << (r - 32)) | (t >>> (64 - r));
        e.rotlBH = b;
        const w = (t, e, r) => (t << (r - 32)) | (e >>> (64 - r));
        function m(t, e, r, n) {
          const o = (e >>> 0) + (n >>> 0);
          return { h: (t + r + ((o / 2 ** 32) | 0)) | 0, l: 0 | o };
        }
        (e.rotlBL = w), (e.add = m);
        const E = (t, e, r) => (t >>> 0) + (e >>> 0) + (r >>> 0);
        e.add3L = E;
        const B = (t, e, r, n) => (e + r + n + ((t / 2 ** 32) | 0)) | 0;
        e.add3H = B;
        const x = (t, e, r, n) => (t >>> 0) + (e >>> 0) + (r >>> 0) + (n >>> 0);
        e.add4L = x;
        const v = (t, e, r, n, o) => (e + r + n + o + ((t / 2 ** 32) | 0)) | 0;
        e.add4H = v;
        const A = (t, e, r, n, o) =>
          (t >>> 0) + (e >>> 0) + (r >>> 0) + (n >>> 0) + (o >>> 0);
        e.add5L = A;
        const I = (t, e, r, n, o, i) =>
          (e + r + n + o + i + ((t / 2 ** 32) | 0)) | 0;
        e.add5H = I;
        const S = {
          fromBig: o,
          split: i,
          toBig: s,
          shrSH: a,
          shrSL: f,
          rotrSH: u,
          rotrSL: c,
          rotrBH: l,
          rotrBL: h,
          rotr32H: d,
          rotr32L: p,
          rotlSH: y,
          rotlSL: g,
          rotlBH: b,
          rotlBL: w,
          add: m,
          add3L: E,
          add3H: B,
          add4L: x,
          add4H: v,
          add5H: I,
          add5L: A,
        };
        e.default = S;
      },
      145: (t, e) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.crypto = void 0),
          (e.crypto =
            "object" == typeof globalThis && "crypto" in globalThis
              ? globalThis.crypto
              : void 0);
      },
      102: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.sha384 =
            e.sha512_256 =
            e.sha512_224 =
            e.sha512 =
            e.SHA512 =
              void 0);
        const n = r(915),
          o = r(318),
          i = r(175),
          [s, a] = (() =>
            o.default.split(
              [
                "0x428a2f98d728ae22",
                "0x7137449123ef65cd",
                "0xb5c0fbcfec4d3b2f",
                "0xe9b5dba58189dbbc",
                "0x3956c25bf348b538",
                "0x59f111f1b605d019",
                "0x923f82a4af194f9b",
                "0xab1c5ed5da6d8118",
                "0xd807aa98a3030242",
                "0x12835b0145706fbe",
                "0x243185be4ee4b28c",
                "0x550c7dc3d5ffb4e2",
                "0x72be5d74f27b896f",
                "0x80deb1fe3b1696b1",
                "0x9bdc06a725c71235",
                "0xc19bf174cf692694",
                "0xe49b69c19ef14ad2",
                "0xefbe4786384f25e3",
                "0x0fc19dc68b8cd5b5",
                "0x240ca1cc77ac9c65",
                "0x2de92c6f592b0275",
                "0x4a7484aa6ea6e483",
                "0x5cb0a9dcbd41fbd4",
                "0x76f988da831153b5",
                "0x983e5152ee66dfab",
                "0xa831c66d2db43210",
                "0xb00327c898fb213f",
                "0xbf597fc7beef0ee4",
                "0xc6e00bf33da88fc2",
                "0xd5a79147930aa725",
                "0x06ca6351e003826f",
                "0x142929670a0e6e70",
                "0x27b70a8546d22ffc",
                "0x2e1b21385c26c926",
                "0x4d2c6dfc5ac42aed",
                "0x53380d139d95b3df",
                "0x650a73548baf63de",
                "0x766a0abb3c77b2a8",
                "0x81c2c92e47edaee6",
                "0x92722c851482353b",
                "0xa2bfe8a14cf10364",
                "0xa81a664bbc423001",
                "0xc24b8b70d0f89791",
                "0xc76c51a30654be30",
                "0xd192e819d6ef5218",
                "0xd69906245565a910",
                "0xf40e35855771202a",
                "0x106aa07032bbd1b8",
                "0x19a4c116b8d2d0c8",
                "0x1e376c085141ab53",
                "0x2748774cdf8eeb99",
                "0x34b0bcb5e19b48a8",
                "0x391c0cb3c5c95a63",
                "0x4ed8aa4ae3418acb",
                "0x5b9cca4f7763e373",
                "0x682e6ff3d6b2b8a3",
                "0x748f82ee5defb2fc",
                "0x78a5636f43172f60",
                "0x84c87814a1f0ab72",
                "0x8cc702081a6439ec",
                "0x90befffa23631e28",
                "0xa4506cebde82bde9",
                "0xbef9a3f7b2c67915",
                "0xc67178f2e372532b",
                "0xca273eceea26619c",
                "0xd186b8c721c0c207",
                "0xeada7dd6cde0eb1e",
                "0xf57d4f7fee6ed178",
                "0x06f067aa72176fba",
                "0x0a637dc5a2c898a6",
                "0x113f9804bef90dae",
                "0x1b710b35131c471b",
                "0x28db77f523047d84",
                "0x32caab7b40c72493",
                "0x3c9ebe0a15c9bebc",
                "0x431d67c49c100d4c",
                "0x4cc5d4becb3e42b6",
                "0x597f299cfc657e2a",
                "0x5fcb6fab3ad6faec",
                "0x6c44198c4a475817",
              ].map((t) => BigInt(t))
            ))(),
          f = new Uint32Array(80),
          u = new Uint32Array(80);
        class c extends n.SHA2 {
          constructor() {
            super(128, 64, 16, !1),
              (this.Ah = 1779033703),
              (this.Al = -205731576),
              (this.Bh = -1150833019),
              (this.Bl = -2067093701),
              (this.Ch = 1013904242),
              (this.Cl = -23791573),
              (this.Dh = -1521486534),
              (this.Dl = 1595750129),
              (this.Eh = 1359893119),
              (this.El = -1377402159),
              (this.Fh = -1694144372),
              (this.Fl = 725511199),
              (this.Gh = 528734635),
              (this.Gl = -79577749),
              (this.Hh = 1541459225),
              (this.Hl = 327033209);
          }
          get() {
            const {
              Ah: t,
              Al: e,
              Bh: r,
              Bl: n,
              Ch: o,
              Cl: i,
              Dh: s,
              Dl: a,
              Eh: f,
              El: u,
              Fh: c,
              Fl: l,
              Gh: h,
              Gl: d,
              Hh: p,
              Hl: y,
            } = this;
            return [t, e, r, n, o, i, s, a, f, u, c, l, h, d, p, y];
          }
          set(t, e, r, n, o, i, s, a, f, u, c, l, h, d, p, y) {
            (this.Ah = 0 | t),
              (this.Al = 0 | e),
              (this.Bh = 0 | r),
              (this.Bl = 0 | n),
              (this.Ch = 0 | o),
              (this.Cl = 0 | i),
              (this.Dh = 0 | s),
              (this.Dl = 0 | a),
              (this.Eh = 0 | f),
              (this.El = 0 | u),
              (this.Fh = 0 | c),
              (this.Fl = 0 | l),
              (this.Gh = 0 | h),
              (this.Gl = 0 | d),
              (this.Hh = 0 | p),
              (this.Hl = 0 | y);
          }
          process(t, e) {
            for (let r = 0; r < 16; r++, e += 4)
              (f[r] = t.getUint32(e)), (u[r] = t.getUint32((e += 4)));
            for (let t = 16; t < 80; t++) {
              const e = 0 | f[t - 15],
                r = 0 | u[t - 15],
                n =
                  o.default.rotrSH(e, r, 1) ^
                  o.default.rotrSH(e, r, 8) ^
                  o.default.shrSH(e, r, 7),
                i =
                  o.default.rotrSL(e, r, 1) ^
                  o.default.rotrSL(e, r, 8) ^
                  o.default.shrSL(e, r, 7),
                s = 0 | f[t - 2],
                a = 0 | u[t - 2],
                c =
                  o.default.rotrSH(s, a, 19) ^
                  o.default.rotrBH(s, a, 61) ^
                  o.default.shrSH(s, a, 6),
                l =
                  o.default.rotrSL(s, a, 19) ^
                  o.default.rotrBL(s, a, 61) ^
                  o.default.shrSL(s, a, 6),
                h = o.default.add4L(i, l, u[t - 7], u[t - 16]),
                d = o.default.add4H(h, n, c, f[t - 7], f[t - 16]);
              (f[t] = 0 | d), (u[t] = 0 | h);
            }
            let {
              Ah: r,
              Al: n,
              Bh: i,
              Bl: c,
              Ch: l,
              Cl: h,
              Dh: d,
              Dl: p,
              Eh: y,
              El: g,
              Fh: b,
              Fl: w,
              Gh: m,
              Gl: E,
              Hh: B,
              Hl: x,
            } = this;
            for (let t = 0; t < 80; t++) {
              const e =
                  o.default.rotrSH(y, g, 14) ^
                  o.default.rotrSH(y, g, 18) ^
                  o.default.rotrBH(y, g, 41),
                v =
                  o.default.rotrSL(y, g, 14) ^
                  o.default.rotrSL(y, g, 18) ^
                  o.default.rotrBL(y, g, 41),
                A = (y & b) ^ (~y & m),
                I = (g & w) ^ (~g & E),
                S = o.default.add5L(x, v, I, a[t], u[t]),
                U = o.default.add5H(S, B, e, A, s[t], f[t]),
                O = 0 | S,
                T =
                  o.default.rotrSH(r, n, 28) ^
                  o.default.rotrBH(r, n, 34) ^
                  o.default.rotrBH(r, n, 39),
                k =
                  o.default.rotrSL(r, n, 28) ^
                  o.default.rotrBL(r, n, 34) ^
                  o.default.rotrBL(r, n, 39),
                L = (r & i) ^ (r & l) ^ (i & l),
                R = (n & c) ^ (n & h) ^ (c & h);
              (B = 0 | m),
                (x = 0 | E),
                (m = 0 | b),
                (E = 0 | w),
                (b = 0 | y),
                (w = 0 | g),
                ({ h: y, l: g } = o.default.add(0 | d, 0 | p, 0 | U, 0 | O)),
                (d = 0 | l),
                (p = 0 | h),
                (l = 0 | i),
                (h = 0 | c),
                (i = 0 | r),
                (c = 0 | n);
              const _ = o.default.add3L(O, k, R);
              (r = o.default.add3H(_, U, T, L)), (n = 0 | _);
            }
            ({ h: r, l: n } = o.default.add(
              0 | this.Ah,
              0 | this.Al,
              0 | r,
              0 | n
            )),
              ({ h: i, l: c } = o.default.add(
                0 | this.Bh,
                0 | this.Bl,
                0 | i,
                0 | c
              )),
              ({ h: l, l: h } = o.default.add(
                0 | this.Ch,
                0 | this.Cl,
                0 | l,
                0 | h
              )),
              ({ h: d, l: p } = o.default.add(
                0 | this.Dh,
                0 | this.Dl,
                0 | d,
                0 | p
              )),
              ({ h: y, l: g } = o.default.add(
                0 | this.Eh,
                0 | this.El,
                0 | y,
                0 | g
              )),
              ({ h: b, l: w } = o.default.add(
                0 | this.Fh,
                0 | this.Fl,
                0 | b,
                0 | w
              )),
              ({ h: m, l: E } = o.default.add(
                0 | this.Gh,
                0 | this.Gl,
                0 | m,
                0 | E
              )),
              ({ h: B, l: x } = o.default.add(
                0 | this.Hh,
                0 | this.Hl,
                0 | B,
                0 | x
              )),
              this.set(r, n, i, c, l, h, d, p, y, g, b, w, m, E, B, x);
          }
          roundClean() {
            f.fill(0), u.fill(0);
          }
          destroy() {
            this.buffer.fill(0),
              this.set(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
          }
        }
        e.SHA512 = c;
        class l extends c {
          constructor() {
            super(),
              (this.Ah = -1942145080),
              (this.Al = 424955298),
              (this.Bh = 1944164710),
              (this.Bl = -1982016298),
              (this.Ch = 502970286),
              (this.Cl = 855612546),
              (this.Dh = 1738396948),
              (this.Dl = 1479516111),
              (this.Eh = 258812777),
              (this.El = 2077511080),
              (this.Fh = 2011393907),
              (this.Fl = 79989058),
              (this.Gh = 1067287976),
              (this.Gl = 1780299464),
              (this.Hh = 286451373),
              (this.Hl = -1848208735),
              (this.outputLen = 28);
          }
        }
        class h extends c {
          constructor() {
            super(),
              (this.Ah = 573645204),
              (this.Al = -64227540),
              (this.Bh = -1621794909),
              (this.Bl = -934517566),
              (this.Ch = 596883563),
              (this.Cl = 1867755857),
              (this.Dh = -1774684391),
              (this.Dl = 1497426621),
              (this.Eh = -1775747358),
              (this.El = -1467023389),
              (this.Fh = -1101128155),
              (this.Fl = 1401305490),
              (this.Gh = 721525244),
              (this.Gl = 746961066),
              (this.Hh = 246885852),
              (this.Hl = -2117784414),
              (this.outputLen = 32);
          }
        }
        class d extends c {
          constructor() {
            super(),
              (this.Ah = -876896931),
              (this.Al = -1056596264),
              (this.Bh = 1654270250),
              (this.Bl = 914150663),
              (this.Ch = -1856437926),
              (this.Cl = 812702999),
              (this.Dh = 355462360),
              (this.Dl = -150054599),
              (this.Eh = 1731405415),
              (this.El = -4191439),
              (this.Fh = -1900787065),
              (this.Fl = 1750603025),
              (this.Gh = -619958771),
              (this.Gl = 1694076839),
              (this.Hh = 1203062813),
              (this.Hl = -1090891868),
              (this.outputLen = 48);
          }
        }
        (e.sha512 = (0, i.wrapConstructor)(() => new c())),
          (e.sha512_224 = (0, i.wrapConstructor)(() => new l())),
          (e.sha512_256 = (0, i.wrapConstructor)(() => new h())),
          (e.sha384 = (0, i.wrapConstructor)(() => new d()));
      },
      175: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.randomBytes =
            e.wrapXOFConstructorWithOpts =
            e.wrapConstructorWithOpts =
            e.wrapConstructor =
            e.checkOpts =
            e.Hash =
            e.concatBytes =
            e.toBytes =
            e.utf8ToBytes =
            e.asyncLoop =
            e.nextTick =
            e.hexToBytes =
            e.bytesToHex =
            e.isLE =
            e.rotr =
            e.createView =
            e.u32 =
            e.u8 =
              void 0);
        const n = r(145);
        function o(t) {
          return (
            t instanceof Uint8Array ||
            (null != t &&
              "object" == typeof t &&
              "Uint8Array" === t.constructor.name)
          );
        }
        if (
          ((e.u8 = (t) => new Uint8Array(t.buffer, t.byteOffset, t.byteLength)),
          (e.u32 = (t) =>
            new Uint32Array(
              t.buffer,
              t.byteOffset,
              Math.floor(t.byteLength / 4)
            )),
          (e.createView = (t) =>
            new DataView(t.buffer, t.byteOffset, t.byteLength)),
          (e.rotr = (t, e) => (t << (32 - e)) | (t >>> e)),
          (e.isLE =
            68 === new Uint8Array(new Uint32Array([287454020]).buffer)[0]),
          !e.isLE)
        )
          throw new Error("Non little-endian hardware is not supported");
        const i = Array.from({ length: 256 }, (t, e) =>
          e.toString(16).padStart(2, "0")
        );
        e.bytesToHex = function (t) {
          if (!o(t)) throw new Error("Uint8Array expected");
          let e = "";
          for (let r = 0; r < t.length; r++) e += i[t[r]];
          return e;
        };
        const s = 48,
          a = 57,
          f = 65,
          u = 70,
          c = 97,
          l = 102;
        function h(t) {
          return t >= s && t <= a
            ? t - s
            : t >= f && t <= u
              ? t - (f - 10)
              : t >= c && t <= l
                ? t - (c - 10)
                : void 0;
        }
        function d(t) {
          if ("string" != typeof t)
            throw new Error("utf8ToBytes expected string, got " + typeof t);
          return new Uint8Array(new TextEncoder().encode(t));
        }
        function p(t) {
          if (("string" == typeof t && (t = d(t)), !o(t)))
            throw new Error("expected Uint8Array, got " + typeof t);
          return t;
        }
        (e.hexToBytes = function (t) {
          if ("string" != typeof t)
            throw new Error("hex string expected, got " + typeof t);
          const e = t.length,
            r = e / 2;
          if (e % 2)
            throw new Error(
              "padded hex string expected, got unpadded hex of length " + e
            );
          const n = new Uint8Array(r);
          for (let e = 0, o = 0; e < r; e++, o += 2) {
            const r = h(t.charCodeAt(o)),
              i = h(t.charCodeAt(o + 1));
            if (void 0 === r || void 0 === i) {
              const e = t[o] + t[o + 1];
              throw new Error(
                'hex string expected, got non-hex character "' +
                  e +
                  '" at index ' +
                  o
              );
            }
            n[e] = 16 * r + i;
          }
          return n;
        }),
          (e.nextTick = async () => {}),
          (e.asyncLoop = async function (t, r, n) {
            let o = Date.now();
            for (let i = 0; i < t; i++) {
              n(i);
              const t = Date.now() - o;
              (t >= 0 && t < r) || (await (0, e.nextTick)(), (o += t));
            }
          }),
          (e.utf8ToBytes = d),
          (e.toBytes = p),
          (e.concatBytes = function (...t) {
            let e = 0;
            for (let r = 0; r < t.length; r++) {
              const n = t[r];
              if (!o(n)) throw new Error("Uint8Array expected");
              e += n.length;
            }
            const r = new Uint8Array(e);
            for (let e = 0, n = 0; e < t.length; e++) {
              const o = t[e];
              r.set(o, n), (n += o.length);
            }
            return r;
          }),
          (e.Hash = class {
            clone() {
              return this._cloneInto();
            }
          });
        const y = {}.toString;
        (e.checkOpts = function (t, e) {
          if (void 0 !== e && "[object Object]" !== y.call(e))
            throw new Error("Options should be object or undefined");
          return Object.assign(t, e);
        }),
          (e.wrapConstructor = function (t) {
            const e = (e) => t().update(p(e)).digest(),
              r = t();
            return (
              (e.outputLen = r.outputLen),
              (e.blockLen = r.blockLen),
              (e.create = () => t()),
              e
            );
          }),
          (e.wrapConstructorWithOpts = function (t) {
            const e = (e, r) => t(r).update(p(e)).digest(),
              r = t({});
            return (
              (e.outputLen = r.outputLen),
              (e.blockLen = r.blockLen),
              (e.create = (e) => t(e)),
              e
            );
          }),
          (e.wrapXOFConstructorWithOpts = function (t) {
            const e = (e, r) => t(r).update(p(e)).digest(),
              r = t({});
            return (
              (e.outputLen = r.outputLen),
              (e.blockLen = r.blockLen),
              (e.create = (e) => t(e)),
              e
            );
          }),
          (e.randomBytes = function (t = 32) {
            if (n.crypto && "function" == typeof n.crypto.getRandomValues)
              return n.crypto.getRandomValues(new Uint8Array(t));
            throw new Error("crypto.getRandomValues must be defined");
          });
      },
      526: (t, e) => {
        "use strict";
        (e.byteLength = function (t) {
          var e = a(t),
            r = e[0],
            n = e[1];
          return (3 * (r + n)) / 4 - n;
        }),
          (e.toByteArray = function (t) {
            var e,
              r,
              i = a(t),
              s = i[0],
              f = i[1],
              u = new o(
                (function (t, e, r) {
                  return (3 * (e + r)) / 4 - r;
                })(0, s, f)
              ),
              c = 0,
              l = f > 0 ? s - 4 : s;
            for (r = 0; r < l; r += 4)
              (e =
                (n[t.charCodeAt(r)] << 18) |
                (n[t.charCodeAt(r + 1)] << 12) |
                (n[t.charCodeAt(r + 2)] << 6) |
                n[t.charCodeAt(r + 3)]),
                (u[c++] = (e >> 16) & 255),
                (u[c++] = (e >> 8) & 255),
                (u[c++] = 255 & e);
            return (
              2 === f &&
                ((e =
                  (n[t.charCodeAt(r)] << 2) | (n[t.charCodeAt(r + 1)] >> 4)),
                (u[c++] = 255 & e)),
              1 === f &&
                ((e =
                  (n[t.charCodeAt(r)] << 10) |
                  (n[t.charCodeAt(r + 1)] << 4) |
                  (n[t.charCodeAt(r + 2)] >> 2)),
                (u[c++] = (e >> 8) & 255),
                (u[c++] = 255 & e)),
              u
            );
          }),
          (e.fromByteArray = function (t) {
            for (
              var e,
                n = t.length,
                o = n % 3,
                i = [],
                s = 16383,
                a = 0,
                u = n - o;
              a < u;
              a += s
            )
              i.push(f(t, a, a + s > u ? u : a + s));
            return (
              1 === o
                ? ((e = t[n - 1]), i.push(r[e >> 2] + r[(e << 4) & 63] + "=="))
                : 2 === o &&
                  ((e = (t[n - 2] << 8) + t[n - 1]),
                  i.push(
                    r[e >> 10] + r[(e >> 4) & 63] + r[(e << 2) & 63] + "="
                  )),
              i.join("")
            );
          });
        for (
          var r = [],
            n = [],
            o = "undefined" != typeof Uint8Array ? Uint8Array : Array,
            i =
              "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
            s = 0;
          s < 64;
          ++s
        )
          (r[s] = i[s]), (n[i.charCodeAt(s)] = s);
        function a(t) {
          var e = t.length;
          if (e % 4 > 0)
            throw new Error("Invalid string. Length must be a multiple of 4");
          var r = t.indexOf("=");
          return -1 === r && (r = e), [r, r === e ? 0 : 4 - (r % 4)];
        }
        function f(t, e, n) {
          for (var o, i, s = [], a = e; a < n; a += 3)
            (o =
              ((t[a] << 16) & 16711680) +
              ((t[a + 1] << 8) & 65280) +
              (255 & t[a + 2])),
              s.push(
                r[((i = o) >> 18) & 63] +
                  r[(i >> 12) & 63] +
                  r[(i >> 6) & 63] +
                  r[63 & i]
              );
          return s.join("");
        }
        (n["-".charCodeAt(0)] = 62), (n["_".charCodeAt(0)] = 63);
      },
      343: (t, e) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.bech32m = e.bech32 = void 0);
        const r = "qpzry9x8gf2tvdw0s3jn54khce6mua7l",
          n = {};
        for (let t = 0; t < 32; t++) {
          const e = r.charAt(t);
          n[e] = t;
        }
        function o(t) {
          const e = t >> 25;
          return (
            ((33554431 & t) << 5) ^
            (996825010 & -((e >> 0) & 1)) ^
            (642813549 & -((e >> 1) & 1)) ^
            (513874426 & -((e >> 2) & 1)) ^
            (1027748829 & -((e >> 3) & 1)) ^
            (705979059 & -((e >> 4) & 1))
          );
        }
        function i(t) {
          let e = 1;
          for (let r = 0; r < t.length; ++r) {
            const n = t.charCodeAt(r);
            if (n < 33 || n > 126) return "Invalid prefix (" + t + ")";
            e = o(e) ^ (n >> 5);
          }
          e = o(e);
          for (let r = 0; r < t.length; ++r) {
            const n = t.charCodeAt(r);
            e = o(e) ^ (31 & n);
          }
          return e;
        }
        function s(t, e, r, n) {
          let o = 0,
            i = 0;
          const s = (1 << r) - 1,
            a = [];
          for (let n = 0; n < t.length; ++n)
            for (o = (o << e) | t[n], i += e; i >= r; )
              (i -= r), a.push((o >> i) & s);
          if (n) i > 0 && a.push((o << (r - i)) & s);
          else {
            if (i >= e) return "Excess padding";
            if ((o << (r - i)) & s) return "Non-zero padding";
          }
          return a;
        }
        function a(t) {
          return s(t, 8, 5, !0);
        }
        function f(t) {
          const e = s(t, 5, 8, !1);
          if (Array.isArray(e)) return e;
        }
        function u(t) {
          const e = s(t, 5, 8, !1);
          if (Array.isArray(e)) return e;
          throw new Error(e);
        }
        function c(t) {
          let e;
          function s(t, r) {
            if (((r = r || 90), t.length < 8)) return t + " too short";
            if (t.length > r) return "Exceeds length limit";
            const s = t.toLowerCase(),
              a = t.toUpperCase();
            if (t !== s && t !== a) return "Mixed-case string " + t;
            const f = (t = s).lastIndexOf("1");
            if (-1 === f) return "No separator character for " + t;
            if (0 === f) return "Missing prefix for " + t;
            const u = t.slice(0, f),
              c = t.slice(f + 1);
            if (c.length < 6) return "Data too short";
            let l = i(u);
            if ("string" == typeof l) return l;
            const h = [];
            for (let t = 0; t < c.length; ++t) {
              const e = c.charAt(t),
                r = n[e];
              if (void 0 === r) return "Unknown character " + e;
              (l = o(l) ^ r), t + 6 >= c.length || h.push(r);
            }
            return l !== e
              ? "Invalid checksum for " + t
              : { prefix: u, words: h };
          }
          return (
            (e = "bech32" === t ? 1 : 734539939),
            {
              decodeUnsafe: function (t, e) {
                const r = s(t, e);
                if ("object" == typeof r) return r;
              },
              decode: function (t, e) {
                const r = s(t, e);
                if ("object" == typeof r) return r;
                throw new Error(r);
              },
              encode: function (t, n, s) {
                if (((s = s || 90), t.length + 7 + n.length > s))
                  throw new TypeError("Exceeds length limit");
                let a = i((t = t.toLowerCase()));
                if ("string" == typeof a) throw new Error(a);
                let f = t + "1";
                for (let t = 0; t < n.length; ++t) {
                  const e = n[t];
                  if (e >> 5 != 0) throw new Error("Non 5-bit word");
                  (a = o(a) ^ e), (f += r.charAt(e));
                }
                for (let t = 0; t < 6; ++t) a = o(a);
                a ^= e;
                for (let t = 0; t < 6; ++t)
                  f += r.charAt((a >> (5 * (5 - t))) & 31);
                return f;
              },
              toWords: a,
              fromWordsUnsafe: f,
              fromWords: u,
            }
          );
        }
        (e.bech32 = c("bech32")), (e.bech32m = c("bech32m"));
      },
      156: (t, e, r) => {
        const n = r(829);
        function o(t, e, r) {
          const n = t[e] + t[r];
          let o = t[e + 1] + t[r + 1];
          n >= 4294967296 && o++, (t[e] = n), (t[e + 1] = o);
        }
        function i(t, e, r, n) {
          let o = t[e] + r;
          r < 0 && (o += 4294967296);
          let i = t[e + 1] + n;
          o >= 4294967296 && i++, (t[e] = o), (t[e + 1] = i);
        }
        function s(t, e) {
          return t[e] ^ (t[e + 1] << 8) ^ (t[e + 2] << 16) ^ (t[e + 3] << 24);
        }
        function a(t, e, r, n, s, a) {
          const f = l[s],
            u = l[s + 1],
            h = l[a],
            d = l[a + 1];
          o(c, t, e), i(c, t, f, u);
          let p = c[n] ^ c[t],
            y = c[n + 1] ^ c[t + 1];
          (c[n] = y),
            (c[n + 1] = p),
            o(c, r, n),
            (p = c[e] ^ c[r]),
            (y = c[e + 1] ^ c[r + 1]),
            (c[e] = (p >>> 24) ^ (y << 8)),
            (c[e + 1] = (y >>> 24) ^ (p << 8)),
            o(c, t, e),
            i(c, t, h, d),
            (p = c[n] ^ c[t]),
            (y = c[n + 1] ^ c[t + 1]),
            (c[n] = (p >>> 16) ^ (y << 16)),
            (c[n + 1] = (y >>> 16) ^ (p << 16)),
            o(c, r, n),
            (p = c[e] ^ c[r]),
            (y = c[e + 1] ^ c[r + 1]),
            (c[e] = (y >>> 31) ^ (p << 1)),
            (c[e + 1] = (p >>> 31) ^ (y << 1));
        }
        const f = new Uint32Array([
            4089235720, 1779033703, 2227873595, 3144134277, 4271175723,
            1013904242, 1595750129, 2773480762, 2917565137, 1359893119,
            725511199, 2600822924, 4215389547, 528734635, 327033209, 1541459225,
          ]),
          u = new Uint8Array(
            [
              0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10, 4,
              8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3, 11, 8, 12, 0, 5, 2, 15,
              13, 10, 14, 3, 6, 7, 1, 9, 4, 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5,
              10, 4, 0, 15, 8, 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3,
              13, 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9, 12, 5,
              1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11, 13, 11, 7, 14, 12,
              1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10, 6, 15, 14, 9, 11, 3, 0, 8, 12,
              2, 13, 7, 1, 4, 10, 5, 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3,
              12, 13, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
              14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3,
            ].map(function (t) {
              return 2 * t;
            })
          ),
          c = new Uint32Array(32),
          l = new Uint32Array(32);
        function h(t, e) {
          let r = 0;
          for (r = 0; r < 16; r++) (c[r] = t.h[r]), (c[r + 16] = f[r]);
          for (
            c[24] = c[24] ^ t.t,
              c[25] = c[25] ^ (t.t / 4294967296),
              e && ((c[28] = ~c[28]), (c[29] = ~c[29])),
              r = 0;
            r < 32;
            r++
          )
            l[r] = s(t.b, 4 * r);
          for (r = 0; r < 12; r++)
            a(0, 8, 16, 24, u[16 * r + 0], u[16 * r + 1]),
              a(2, 10, 18, 26, u[16 * r + 2], u[16 * r + 3]),
              a(4, 12, 20, 28, u[16 * r + 4], u[16 * r + 5]),
              a(6, 14, 22, 30, u[16 * r + 6], u[16 * r + 7]),
              a(0, 10, 20, 30, u[16 * r + 8], u[16 * r + 9]),
              a(2, 12, 22, 24, u[16 * r + 10], u[16 * r + 11]),
              a(4, 14, 16, 26, u[16 * r + 12], u[16 * r + 13]),
              a(6, 8, 18, 28, u[16 * r + 14], u[16 * r + 15]);
          for (r = 0; r < 16; r++) t.h[r] = t.h[r] ^ c[r] ^ c[r + 16];
        }
        const d = new Uint8Array([
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ]);
        function p(t, e, r, n) {
          if (0 === t || t > 64)
            throw new Error("Illegal output length, expected 0 < length <= 64");
          if (e && e.length > 64)
            throw new Error(
              "Illegal key, expected Uint8Array with 0 < length <= 64"
            );
          if (r && 16 !== r.length)
            throw new Error(
              "Illegal salt, expected Uint8Array with length is 16"
            );
          if (n && 16 !== n.length)
            throw new Error(
              "Illegal personal, expected Uint8Array with length is 16"
            );
          const o = {
            b: new Uint8Array(128),
            h: new Uint32Array(16),
            t: 0,
            c: 0,
            outlen: t,
          };
          d.fill(0),
            (d[0] = t),
            e && (d[1] = e.length),
            (d[2] = 1),
            (d[3] = 1),
            r && d.set(r, 32),
            n && d.set(n, 48);
          for (let t = 0; t < 16; t++) o.h[t] = f[t] ^ s(d, 4 * t);
          return e && (y(o, e), (o.c = 128)), o;
        }
        function y(t, e) {
          for (let r = 0; r < e.length; r++)
            128 === t.c && ((t.t += t.c), h(t, !1), (t.c = 0)),
              (t.b[t.c++] = e[r]);
        }
        function g(t) {
          for (t.t += t.c; t.c < 128; ) t.b[t.c++] = 0;
          h(t, !0);
          const e = new Uint8Array(t.outlen);
          for (let r = 0; r < t.outlen; r++)
            e[r] = t.h[r >> 2] >> (8 * (3 & r));
          return e;
        }
        function b(t, e, r, o, i) {
          (r = r || 64),
            (t = n.normalizeInput(t)),
            o && (o = n.normalizeInput(o)),
            i && (i = n.normalizeInput(i));
          const s = p(r, e, o, i);
          return y(s, t), g(s);
        }
        t.exports = {
          blake2b: b,
          blake2bHex: function (t, e, r, o, i) {
            const s = b(t, e, r, o, i);
            return n.toHex(s);
          },
          blake2bInit: p,
          blake2bUpdate: y,
          blake2bFinal: g,
        };
      },
      843: (t, e, r) => {
        const n = r(829);
        function o(t, e) {
          return t[e] ^ (t[e + 1] << 8) ^ (t[e + 2] << 16) ^ (t[e + 3] << 24);
        }
        function i(t, e, r, n, o, i) {
          (u[t] = u[t] + u[e] + o),
            (u[n] = s(u[n] ^ u[t], 16)),
            (u[r] = u[r] + u[n]),
            (u[e] = s(u[e] ^ u[r], 12)),
            (u[t] = u[t] + u[e] + i),
            (u[n] = s(u[n] ^ u[t], 8)),
            (u[r] = u[r] + u[n]),
            (u[e] = s(u[e] ^ u[r], 7));
        }
        function s(t, e) {
          return (t >>> e) ^ (t << (32 - e));
        }
        const a = new Uint32Array([
            1779033703, 3144134277, 1013904242, 2773480762, 1359893119,
            2600822924, 528734635, 1541459225,
          ]),
          f = new Uint8Array([
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10, 4, 8,
            9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3, 11, 8, 12, 0, 5, 2, 15, 13,
            10, 14, 3, 6, 7, 1, 9, 4, 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10,
            4, 0, 15, 8, 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13,
            2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9, 12, 5, 1, 15,
            14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11, 13, 11, 7, 14, 12, 1, 3, 9,
            5, 0, 15, 4, 8, 6, 2, 10, 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7,
            1, 4, 10, 5, 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0,
          ]),
          u = new Uint32Array(16),
          c = new Uint32Array(16);
        function l(t, e) {
          let r = 0;
          for (r = 0; r < 8; r++) (u[r] = t.h[r]), (u[r + 8] = a[r]);
          for (
            u[12] ^= t.t,
              u[13] ^= t.t / 4294967296,
              e && (u[14] = ~u[14]),
              r = 0;
            r < 16;
            r++
          )
            c[r] = o(t.b, 4 * r);
          for (r = 0; r < 10; r++)
            i(0, 4, 8, 12, c[f[16 * r + 0]], c[f[16 * r + 1]]),
              i(1, 5, 9, 13, c[f[16 * r + 2]], c[f[16 * r + 3]]),
              i(2, 6, 10, 14, c[f[16 * r + 4]], c[f[16 * r + 5]]),
              i(3, 7, 11, 15, c[f[16 * r + 6]], c[f[16 * r + 7]]),
              i(0, 5, 10, 15, c[f[16 * r + 8]], c[f[16 * r + 9]]),
              i(1, 6, 11, 12, c[f[16 * r + 10]], c[f[16 * r + 11]]),
              i(2, 7, 8, 13, c[f[16 * r + 12]], c[f[16 * r + 13]]),
              i(3, 4, 9, 14, c[f[16 * r + 14]], c[f[16 * r + 15]]);
          for (r = 0; r < 8; r++) t.h[r] ^= u[r] ^ u[r + 8];
        }
        function h(t, e) {
          if (!(t > 0 && t <= 32))
            throw new Error("Incorrect output length, should be in [1, 32]");
          const r = e ? e.length : 0;
          if (e && !(r > 0 && r <= 32))
            throw new Error("Incorrect key length, should be in [1, 32]");
          const n = {
            h: new Uint32Array(a),
            b: new Uint8Array(64),
            c: 0,
            t: 0,
            outlen: t,
          };
          return (
            (n.h[0] ^= 16842752 ^ (r << 8) ^ t),
            r > 0 && (d(n, e), (n.c = 64)),
            n
          );
        }
        function d(t, e) {
          for (let r = 0; r < e.length; r++)
            64 === t.c && ((t.t += t.c), l(t, !1), (t.c = 0)),
              (t.b[t.c++] = e[r]);
        }
        function p(t) {
          for (t.t += t.c; t.c < 64; ) t.b[t.c++] = 0;
          l(t, !0);
          const e = new Uint8Array(t.outlen);
          for (let r = 0; r < t.outlen; r++)
            e[r] = (t.h[r >> 2] >> (8 * (3 & r))) & 255;
          return e;
        }
        function y(t, e, r) {
          (r = r || 32), (t = n.normalizeInput(t));
          const o = h(r, e);
          return d(o, t), p(o);
        }
        t.exports = {
          blake2s: y,
          blake2sHex: function (t, e, r) {
            const o = y(t, e, r);
            return n.toHex(o);
          },
          blake2sInit: h,
          blake2sUpdate: d,
          blake2sFinal: p,
        };
      },
      493: (t, e, r) => {
        const n = r(156),
          o = r(843);
        t.exports = {
          blake2b: n.blake2b,
          blake2bHex: n.blake2bHex,
          blake2bInit: n.blake2bInit,
          blake2bUpdate: n.blake2bUpdate,
          blake2bFinal: n.blake2bFinal,
          blake2s: o.blake2s,
          blake2sHex: o.blake2sHex,
          blake2sInit: o.blake2sInit,
          blake2sUpdate: o.blake2sUpdate,
          blake2sFinal: o.blake2sFinal,
        };
      },
      829: (t) => {
        function e(t) {
          return (4294967296 + t).toString(16).substring(1);
        }
        t.exports = {
          normalizeInput: function (t) {
            let e;
            if (t instanceof Uint8Array) e = t;
            else {
              if ("string" != typeof t)
                throw new Error(
                  "Input must be an string, Buffer or Uint8Array"
                );
              e = new TextEncoder().encode(t);
            }
            return e;
          },
          toHex: function (t) {
            return Array.prototype.map
              .call(t, function (t) {
                return (t < 16 ? "0" : "") + t.toString(16);
              })
              .join("");
          },
          debugPrint: function (t, r, n) {
            let o = "\n" + t + " = ";
            for (let i = 0; i < r.length; i += 2) {
              if (32 === n)
                (o += e(r[i]).toUpperCase()),
                  (o += " "),
                  (o += e(r[i + 1]).toUpperCase());
              else {
                if (64 !== n) throw new Error("Invalid size " + n);
                (o += e(r[i + 1]).toUpperCase()), (o += e(r[i]).toUpperCase());
              }
              i % 6 == 4
                ? (o += "\n" + new Array(t.length + 4).join(" "))
                : i < r.length - 2 && (o += " ");
            }
            console.log(o);
          },
          testSpeed: function (t, e, r) {
            let n = new Date().getTime();
            const o = new Uint8Array(e);
            for (let t = 0; t < e; t++) o[t] = t % 256;
            const i = new Date().getTime();
            console.log("Generated random input in " + (i - n) + "ms"), (n = i);
            for (let i = 0; i < r; i++) {
              const r = t(o),
                i = new Date().getTime(),
                s = i - n;
              (n = i),
                console.log(
                  "Hashed in " + s + "ms: " + r.substring(0, 20) + "..."
                ),
                console.log(
                  Math.round((e / (1 << 20) / (s / 1e3)) * 100) / 100 +
                    " MB PER SECOND"
                );
            }
          },
        };
      },
      287: (t, e, r) => {
        "use strict";
        const n = r(526),
          o = r(251),
          i =
            "function" == typeof Symbol && "function" == typeof Symbol.for
              ? Symbol.for("nodejs.util.inspect.custom")
              : null;
        (e.Buffer = f),
          (e.SlowBuffer = function (t) {
            return +t != t && (t = 0), f.alloc(+t);
          }),
          (e.INSPECT_MAX_BYTES = 50);
        const s = 2147483647;
        function a(t) {
          if (t > s)
            throw new RangeError(
              'The value "' + t + '" is invalid for option "size"'
            );
          const e = new Uint8Array(t);
          return Object.setPrototypeOf(e, f.prototype), e;
        }
        function f(t, e, r) {
          if ("number" == typeof t) {
            if ("string" == typeof e)
              throw new TypeError(
                'The "string" argument must be of type string. Received type number'
              );
            return l(t);
          }
          return u(t, e, r);
        }
        function u(t, e, r) {
          if ("string" == typeof t)
            return (function (t, e) {
              if (
                (("string" == typeof e && "" !== e) || (e = "utf8"),
                !f.isEncoding(e))
              )
                throw new TypeError("Unknown encoding: " + e);
              const r = 0 | y(t, e);
              let n = a(r);
              const o = n.write(t, e);
              return o !== r && (n = n.slice(0, o)), n;
            })(t, e);
          if (ArrayBuffer.isView(t))
            return (function (t) {
              if (Z(t, Uint8Array)) {
                const e = new Uint8Array(t);
                return d(e.buffer, e.byteOffset, e.byteLength);
              }
              return h(t);
            })(t);
          if (null == t)
            throw new TypeError(
              "The first argument must be one of type string, Buffer, ArrayBuffer, Array, or Array-like Object. Received type " +
                typeof t
            );
          if (Z(t, ArrayBuffer) || (t && Z(t.buffer, ArrayBuffer)))
            return d(t, e, r);
          if (
            "undefined" != typeof SharedArrayBuffer &&
            (Z(t, SharedArrayBuffer) || (t && Z(t.buffer, SharedArrayBuffer)))
          )
            return d(t, e, r);
          if ("number" == typeof t)
            throw new TypeError(
              'The "value" argument must not be of type number. Received type number'
            );
          const n = t.valueOf && t.valueOf();
          if (null != n && n !== t) return f.from(n, e, r);
          const o = (function (t) {
            if (f.isBuffer(t)) {
              const e = 0 | p(t.length),
                r = a(e);
              return 0 === r.length || t.copy(r, 0, 0, e), r;
            }
            return void 0 !== t.length
              ? "number" != typeof t.length || J(t.length)
                ? a(0)
                : h(t)
              : "Buffer" === t.type && Array.isArray(t.data)
                ? h(t.data)
                : void 0;
          })(t);
          if (o) return o;
          if (
            "undefined" != typeof Symbol &&
            null != Symbol.toPrimitive &&
            "function" == typeof t[Symbol.toPrimitive]
          )
            return f.from(t[Symbol.toPrimitive]("string"), e, r);
          throw new TypeError(
            "The first argument must be one of type string, Buffer, ArrayBuffer, Array, or Array-like Object. Received type " +
              typeof t
          );
        }
        function c(t) {
          if ("number" != typeof t)
            throw new TypeError('"size" argument must be of type number');
          if (t < 0)
            throw new RangeError(
              'The value "' + t + '" is invalid for option "size"'
            );
        }
        function l(t) {
          return c(t), a(t < 0 ? 0 : 0 | p(t));
        }
        function h(t) {
          const e = t.length < 0 ? 0 : 0 | p(t.length),
            r = a(e);
          for (let n = 0; n < e; n += 1) r[n] = 255 & t[n];
          return r;
        }
        function d(t, e, r) {
          if (e < 0 || t.byteLength < e)
            throw new RangeError('"offset" is outside of buffer bounds');
          if (t.byteLength < e + (r || 0))
            throw new RangeError('"length" is outside of buffer bounds');
          let n;
          return (
            (n =
              void 0 === e && void 0 === r
                ? new Uint8Array(t)
                : void 0 === r
                  ? new Uint8Array(t, e)
                  : new Uint8Array(t, e, r)),
            Object.setPrototypeOf(n, f.prototype),
            n
          );
        }
        function p(t) {
          if (t >= s)
            throw new RangeError(
              "Attempt to allocate Buffer larger than maximum size: 0x" +
                s.toString(16) +
                " bytes"
            );
          return 0 | t;
        }
        function y(t, e) {
          if (f.isBuffer(t)) return t.length;
          if (ArrayBuffer.isView(t) || Z(t, ArrayBuffer)) return t.byteLength;
          if ("string" != typeof t)
            throw new TypeError(
              'The "string" argument must be one of type string, Buffer, or ArrayBuffer. Received type ' +
                typeof t
            );
          const r = t.length,
            n = arguments.length > 2 && !0 === arguments[2];
          if (!n && 0 === r) return 0;
          let o = !1;
          for (;;)
            switch (e) {
              case "ascii":
              case "latin1":
              case "binary":
                return r;
              case "utf8":
              case "utf-8":
                return W(t).length;
              case "ucs2":
              case "ucs-2":
              case "utf16le":
              case "utf-16le":
                return 2 * r;
              case "hex":
                return r >>> 1;
              case "base64":
                return z(t).length;
              default:
                if (o) return n ? -1 : W(t).length;
                (e = ("" + e).toLowerCase()), (o = !0);
            }
        }
        function g(t, e, r) {
          let n = !1;
          if (((void 0 === e || e < 0) && (e = 0), e > this.length)) return "";
          if (((void 0 === r || r > this.length) && (r = this.length), r <= 0))
            return "";
          if ((r >>>= 0) <= (e >>>= 0)) return "";
          for (t || (t = "utf8"); ; )
            switch (t) {
              case "hex":
                return k(this, e, r);
              case "utf8":
              case "utf-8":
                return S(this, e, r);
              case "ascii":
                return O(this, e, r);
              case "latin1":
              case "binary":
                return T(this, e, r);
              case "base64":
                return I(this, e, r);
              case "ucs2":
              case "ucs-2":
              case "utf16le":
              case "utf-16le":
                return L(this, e, r);
              default:
                if (n) throw new TypeError("Unknown encoding: " + t);
                (t = (t + "").toLowerCase()), (n = !0);
            }
        }
        function b(t, e, r) {
          const n = t[e];
          (t[e] = t[r]), (t[r] = n);
        }
        function w(t, e, r, n, o) {
          if (0 === t.length) return -1;
          if (
            ("string" == typeof r
              ? ((n = r), (r = 0))
              : r > 2147483647
                ? (r = 2147483647)
                : r < -2147483648 && (r = -2147483648),
            J((r = +r)) && (r = o ? 0 : t.length - 1),
            r < 0 && (r = t.length + r),
            r >= t.length)
          ) {
            if (o) return -1;
            r = t.length - 1;
          } else if (r < 0) {
            if (!o) return -1;
            r = 0;
          }
          if (("string" == typeof e && (e = f.from(e, n)), f.isBuffer(e)))
            return 0 === e.length ? -1 : m(t, e, r, n, o);
          if ("number" == typeof e)
            return (
              (e &= 255),
              "function" == typeof Uint8Array.prototype.indexOf
                ? o
                  ? Uint8Array.prototype.indexOf.call(t, e, r)
                  : Uint8Array.prototype.lastIndexOf.call(t, e, r)
                : m(t, [e], r, n, o)
            );
          throw new TypeError("val must be string, number or Buffer");
        }
        function m(t, e, r, n, o) {
          let i,
            s = 1,
            a = t.length,
            f = e.length;
          if (
            void 0 !== n &&
            ("ucs2" === (n = String(n).toLowerCase()) ||
              "ucs-2" === n ||
              "utf16le" === n ||
              "utf-16le" === n)
          ) {
            if (t.length < 2 || e.length < 2) return -1;
            (s = 2), (a /= 2), (f /= 2), (r /= 2);
          }
          function u(t, e) {
            return 1 === s ? t[e] : t.readUInt16BE(e * s);
          }
          if (o) {
            let n = -1;
            for (i = r; i < a; i++)
              if (u(t, i) === u(e, -1 === n ? 0 : i - n)) {
                if ((-1 === n && (n = i), i - n + 1 === f)) return n * s;
              } else -1 !== n && (i -= i - n), (n = -1);
          } else
            for (r + f > a && (r = a - f), i = r; i >= 0; i--) {
              let r = !0;
              for (let n = 0; n < f; n++)
                if (u(t, i + n) !== u(e, n)) {
                  r = !1;
                  break;
                }
              if (r) return i;
            }
          return -1;
        }
        function E(t, e, r, n) {
          r = Number(r) || 0;
          const o = t.length - r;
          n ? (n = Number(n)) > o && (n = o) : (n = o);
          const i = e.length;
          let s;
          for (n > i / 2 && (n = i / 2), s = 0; s < n; ++s) {
            const n = parseInt(e.substr(2 * s, 2), 16);
            if (J(n)) return s;
            t[r + s] = n;
          }
          return s;
        }
        function B(t, e, r, n) {
          return G(W(e, t.length - r), t, r, n);
        }
        function x(t, e, r, n) {
          return G(
            (function (t) {
              const e = [];
              for (let r = 0; r < t.length; ++r) e.push(255 & t.charCodeAt(r));
              return e;
            })(e),
            t,
            r,
            n
          );
        }
        function v(t, e, r, n) {
          return G(z(e), t, r, n);
        }
        function A(t, e, r, n) {
          return G(
            (function (t, e) {
              let r, n, o;
              const i = [];
              for (let s = 0; s < t.length && !((e -= 2) < 0); ++s)
                (r = t.charCodeAt(s)),
                  (n = r >> 8),
                  (o = r % 256),
                  i.push(o),
                  i.push(n);
              return i;
            })(e, t.length - r),
            t,
            r,
            n
          );
        }
        function I(t, e, r) {
          return 0 === e && r === t.length
            ? n.fromByteArray(t)
            : n.fromByteArray(t.slice(e, r));
        }
        function S(t, e, r) {
          r = Math.min(t.length, r);
          const n = [];
          let o = e;
          for (; o < r; ) {
            const e = t[o];
            let i = null,
              s = e > 239 ? 4 : e > 223 ? 3 : e > 191 ? 2 : 1;
            if (o + s <= r) {
              let r, n, a, f;
              switch (s) {
                case 1:
                  e < 128 && (i = e);
                  break;
                case 2:
                  (r = t[o + 1]),
                    128 == (192 & r) &&
                      ((f = ((31 & e) << 6) | (63 & r)), f > 127 && (i = f));
                  break;
                case 3:
                  (r = t[o + 1]),
                    (n = t[o + 2]),
                    128 == (192 & r) &&
                      128 == (192 & n) &&
                      ((f = ((15 & e) << 12) | ((63 & r) << 6) | (63 & n)),
                      f > 2047 && (f < 55296 || f > 57343) && (i = f));
                  break;
                case 4:
                  (r = t[o + 1]),
                    (n = t[o + 2]),
                    (a = t[o + 3]),
                    128 == (192 & r) &&
                      128 == (192 & n) &&
                      128 == (192 & a) &&
                      ((f =
                        ((15 & e) << 18) |
                        ((63 & r) << 12) |
                        ((63 & n) << 6) |
                        (63 & a)),
                      f > 65535 && f < 1114112 && (i = f));
              }
            }
            null === i
              ? ((i = 65533), (s = 1))
              : i > 65535 &&
                ((i -= 65536),
                n.push(((i >>> 10) & 1023) | 55296),
                (i = 56320 | (1023 & i))),
              n.push(i),
              (o += s);
          }
          return (function (t) {
            const e = t.length;
            if (e <= U) return String.fromCharCode.apply(String, t);
            let r = "",
              n = 0;
            for (; n < e; )
              r += String.fromCharCode.apply(String, t.slice(n, (n += U)));
            return r;
          })(n);
        }
        (e.kMaxLength = s),
          (f.TYPED_ARRAY_SUPPORT = (function () {
            try {
              const t = new Uint8Array(1),
                e = {
                  foo: function () {
                    return 42;
                  },
                };
              return (
                Object.setPrototypeOf(e, Uint8Array.prototype),
                Object.setPrototypeOf(t, e),
                42 === t.foo()
              );
            } catch (t) {
              return !1;
            }
          })()),
          f.TYPED_ARRAY_SUPPORT ||
            "undefined" == typeof console ||
            "function" != typeof console.error ||
            console.error(
              "This browser lacks typed array (Uint8Array) support which is required by `buffer` v5.x. Use `buffer` v4.x if you require old browser support."
            ),
          Object.defineProperty(f.prototype, "parent", {
            enumerable: !0,
            get: function () {
              if (f.isBuffer(this)) return this.buffer;
            },
          }),
          Object.defineProperty(f.prototype, "offset", {
            enumerable: !0,
            get: function () {
              if (f.isBuffer(this)) return this.byteOffset;
            },
          }),
          (f.poolSize = 8192),
          (f.from = function (t, e, r) {
            return u(t, e, r);
          }),
          Object.setPrototypeOf(f.prototype, Uint8Array.prototype),
          Object.setPrototypeOf(f, Uint8Array),
          (f.alloc = function (t, e, r) {
            return (function (t, e, r) {
              return (
                c(t),
                t <= 0
                  ? a(t)
                  : void 0 !== e
                    ? "string" == typeof r
                      ? a(t).fill(e, r)
                      : a(t).fill(e)
                    : a(t)
              );
            })(t, e, r);
          }),
          (f.allocUnsafe = function (t) {
            return l(t);
          }),
          (f.allocUnsafeSlow = function (t) {
            return l(t);
          }),
          (f.isBuffer = function (t) {
            return null != t && !0 === t._isBuffer && t !== f.prototype;
          }),
          (f.compare = function (t, e) {
            if (
              (Z(t, Uint8Array) && (t = f.from(t, t.offset, t.byteLength)),
              Z(e, Uint8Array) && (e = f.from(e, e.offset, e.byteLength)),
              !f.isBuffer(t) || !f.isBuffer(e))
            )
              throw new TypeError(
                'The "buf1", "buf2" arguments must be one of type Buffer or Uint8Array'
              );
            if (t === e) return 0;
            let r = t.length,
              n = e.length;
            for (let o = 0, i = Math.min(r, n); o < i; ++o)
              if (t[o] !== e[o]) {
                (r = t[o]), (n = e[o]);
                break;
              }
            return r < n ? -1 : n < r ? 1 : 0;
          }),
          (f.isEncoding = function (t) {
            switch (String(t).toLowerCase()) {
              case "hex":
              case "utf8":
              case "utf-8":
              case "ascii":
              case "latin1":
              case "binary":
              case "base64":
              case "ucs2":
              case "ucs-2":
              case "utf16le":
              case "utf-16le":
                return !0;
              default:
                return !1;
            }
          }),
          (f.concat = function (t, e) {
            if (!Array.isArray(t))
              throw new TypeError(
                '"list" argument must be an Array of Buffers'
              );
            if (0 === t.length) return f.alloc(0);
            let r;
            if (void 0 === e)
              for (e = 0, r = 0; r < t.length; ++r) e += t[r].length;
            const n = f.allocUnsafe(e);
            let o = 0;
            for (r = 0; r < t.length; ++r) {
              let e = t[r];
              if (Z(e, Uint8Array))
                o + e.length > n.length
                  ? (f.isBuffer(e) || (e = f.from(e)), e.copy(n, o))
                  : Uint8Array.prototype.set.call(n, e, o);
              else {
                if (!f.isBuffer(e))
                  throw new TypeError(
                    '"list" argument must be an Array of Buffers'
                  );
                e.copy(n, o);
              }
              o += e.length;
            }
            return n;
          }),
          (f.byteLength = y),
          (f.prototype._isBuffer = !0),
          (f.prototype.swap16 = function () {
            const t = this.length;
            if (t % 2 != 0)
              throw new RangeError("Buffer size must be a multiple of 16-bits");
            for (let e = 0; e < t; e += 2) b(this, e, e + 1);
            return this;
          }),
          (f.prototype.swap32 = function () {
            const t = this.length;
            if (t % 4 != 0)
              throw new RangeError("Buffer size must be a multiple of 32-bits");
            for (let e = 0; e < t; e += 4)
              b(this, e, e + 3), b(this, e + 1, e + 2);
            return this;
          }),
          (f.prototype.swap64 = function () {
            const t = this.length;
            if (t % 8 != 0)
              throw new RangeError("Buffer size must be a multiple of 64-bits");
            for (let e = 0; e < t; e += 8)
              b(this, e, e + 7),
                b(this, e + 1, e + 6),
                b(this, e + 2, e + 5),
                b(this, e + 3, e + 4);
            return this;
          }),
          (f.prototype.toString = function () {
            const t = this.length;
            return 0 === t
              ? ""
              : 0 === arguments.length
                ? S(this, 0, t)
                : g.apply(this, arguments);
          }),
          (f.prototype.toLocaleString = f.prototype.toString),
          (f.prototype.equals = function (t) {
            if (!f.isBuffer(t))
              throw new TypeError("Argument must be a Buffer");
            return this === t || 0 === f.compare(this, t);
          }),
          (f.prototype.inspect = function () {
            let t = "";
            const r = e.INSPECT_MAX_BYTES;
            return (
              (t = this.toString("hex", 0, r)
                .replace(/(.{2})/g, "$1 ")
                .trim()),
              this.length > r && (t += " ... "),
              "<Buffer " + t + ">"
            );
          }),
          i && (f.prototype[i] = f.prototype.inspect),
          (f.prototype.compare = function (t, e, r, n, o) {
            if (
              (Z(t, Uint8Array) && (t = f.from(t, t.offset, t.byteLength)),
              !f.isBuffer(t))
            )
              throw new TypeError(
                'The "target" argument must be one of type Buffer or Uint8Array. Received type ' +
                  typeof t
              );
            if (
              (void 0 === e && (e = 0),
              void 0 === r && (r = t ? t.length : 0),
              void 0 === n && (n = 0),
              void 0 === o && (o = this.length),
              e < 0 || r > t.length || n < 0 || o > this.length)
            )
              throw new RangeError("out of range index");
            if (n >= o && e >= r) return 0;
            if (n >= o) return -1;
            if (e >= r) return 1;
            if (this === t) return 0;
            let i = (o >>>= 0) - (n >>>= 0),
              s = (r >>>= 0) - (e >>>= 0);
            const a = Math.min(i, s),
              u = this.slice(n, o),
              c = t.slice(e, r);
            for (let t = 0; t < a; ++t)
              if (u[t] !== c[t]) {
                (i = u[t]), (s = c[t]);
                break;
              }
            return i < s ? -1 : s < i ? 1 : 0;
          }),
          (f.prototype.includes = function (t, e, r) {
            return -1 !== this.indexOf(t, e, r);
          }),
          (f.prototype.indexOf = function (t, e, r) {
            return w(this, t, e, r, !0);
          }),
          (f.prototype.lastIndexOf = function (t, e, r) {
            return w(this, t, e, r, !1);
          }),
          (f.prototype.write = function (t, e, r, n) {
            if (void 0 === e) (n = "utf8"), (r = this.length), (e = 0);
            else if (void 0 === r && "string" == typeof e)
              (n = e), (r = this.length), (e = 0);
            else {
              if (!isFinite(e))
                throw new Error(
                  "Buffer.write(string, encoding, offset[, length]) is no longer supported"
                );
              (e >>>= 0),
                isFinite(r)
                  ? ((r >>>= 0), void 0 === n && (n = "utf8"))
                  : ((n = r), (r = void 0));
            }
            const o = this.length - e;
            if (
              ((void 0 === r || r > o) && (r = o),
              (t.length > 0 && (r < 0 || e < 0)) || e > this.length)
            )
              throw new RangeError("Attempt to write outside buffer bounds");
            n || (n = "utf8");
            let i = !1;
            for (;;)
              switch (n) {
                case "hex":
                  return E(this, t, e, r);
                case "utf8":
                case "utf-8":
                  return B(this, t, e, r);
                case "ascii":
                case "latin1":
                case "binary":
                  return x(this, t, e, r);
                case "base64":
                  return v(this, t, e, r);
                case "ucs2":
                case "ucs-2":
                case "utf16le":
                case "utf-16le":
                  return A(this, t, e, r);
                default:
                  if (i) throw new TypeError("Unknown encoding: " + n);
                  (n = ("" + n).toLowerCase()), (i = !0);
              }
          }),
          (f.prototype.toJSON = function () {
            return {
              type: "Buffer",
              data: Array.prototype.slice.call(this._arr || this, 0),
            };
          });
        const U = 4096;
        function O(t, e, r) {
          let n = "";
          r = Math.min(t.length, r);
          for (let o = e; o < r; ++o) n += String.fromCharCode(127 & t[o]);
          return n;
        }
        function T(t, e, r) {
          let n = "";
          r = Math.min(t.length, r);
          for (let o = e; o < r; ++o) n += String.fromCharCode(t[o]);
          return n;
        }
        function k(t, e, r) {
          const n = t.length;
          (!e || e < 0) && (e = 0), (!r || r < 0 || r > n) && (r = n);
          let o = "";
          for (let n = e; n < r; ++n) o += Y[t[n]];
          return o;
        }
        function L(t, e, r) {
          const n = t.slice(e, r);
          let o = "";
          for (let t = 0; t < n.length - 1; t += 2)
            o += String.fromCharCode(n[t] + 256 * n[t + 1]);
          return o;
        }
        function R(t, e, r) {
          if (t % 1 != 0 || t < 0) throw new RangeError("offset is not uint");
          if (t + e > r)
            throw new RangeError("Trying to access beyond buffer length");
        }
        function _(t, e, r, n, o, i) {
          if (!f.isBuffer(t))
            throw new TypeError('"buffer" argument must be a Buffer instance');
          if (e > o || e < i)
            throw new RangeError('"value" argument is out of bounds');
          if (r + n > t.length) throw new RangeError("Index out of range");
        }
        function M(t, e, r, n, o) {
          V(e, n, o, t, r, 7);
          let i = Number(e & BigInt(4294967295));
          (t[r++] = i),
            (i >>= 8),
            (t[r++] = i),
            (i >>= 8),
            (t[r++] = i),
            (i >>= 8),
            (t[r++] = i);
          let s = Number((e >> BigInt(32)) & BigInt(4294967295));
          return (
            (t[r++] = s),
            (s >>= 8),
            (t[r++] = s),
            (s >>= 8),
            (t[r++] = s),
            (s >>= 8),
            (t[r++] = s),
            r
          );
        }
        function C(t, e, r, n, o) {
          V(e, n, o, t, r, 7);
          let i = Number(e & BigInt(4294967295));
          (t[r + 7] = i),
            (i >>= 8),
            (t[r + 6] = i),
            (i >>= 8),
            (t[r + 5] = i),
            (i >>= 8),
            (t[r + 4] = i);
          let s = Number((e >> BigInt(32)) & BigInt(4294967295));
          return (
            (t[r + 3] = s),
            (s >>= 8),
            (t[r + 2] = s),
            (s >>= 8),
            (t[r + 1] = s),
            (s >>= 8),
            (t[r] = s),
            r + 8
          );
        }
        function j(t, e, r, n, o, i) {
          if (r + n > t.length) throw new RangeError("Index out of range");
          if (r < 0) throw new RangeError("Index out of range");
        }
        function N(t, e, r, n, i) {
          return (
            (e = +e),
            (r >>>= 0),
            i || j(t, 0, r, 4),
            o.write(t, e, r, n, 23, 4),
            r + 4
          );
        }
        function P(t, e, r, n, i) {
          return (
            (e = +e),
            (r >>>= 0),
            i || j(t, 0, r, 8),
            o.write(t, e, r, n, 52, 8),
            r + 8
          );
        }
        (f.prototype.slice = function (t, e) {
          const r = this.length;
          (t = ~~t) < 0 ? (t += r) < 0 && (t = 0) : t > r && (t = r),
            (e = void 0 === e ? r : ~~e) < 0
              ? (e += r) < 0 && (e = 0)
              : e > r && (e = r),
            e < t && (e = t);
          const n = this.subarray(t, e);
          return Object.setPrototypeOf(n, f.prototype), n;
        }),
          (f.prototype.readUintLE = f.prototype.readUIntLE =
            function (t, e, r) {
              (t >>>= 0), (e >>>= 0), r || R(t, e, this.length);
              let n = this[t],
                o = 1,
                i = 0;
              for (; ++i < e && (o *= 256); ) n += this[t + i] * o;
              return n;
            }),
          (f.prototype.readUintBE = f.prototype.readUIntBE =
            function (t, e, r) {
              (t >>>= 0), (e >>>= 0), r || R(t, e, this.length);
              let n = this[t + --e],
                o = 1;
              for (; e > 0 && (o *= 256); ) n += this[t + --e] * o;
              return n;
            }),
          (f.prototype.readUint8 = f.prototype.readUInt8 =
            function (t, e) {
              return (t >>>= 0), e || R(t, 1, this.length), this[t];
            }),
          (f.prototype.readUint16LE = f.prototype.readUInt16LE =
            function (t, e) {
              return (
                (t >>>= 0),
                e || R(t, 2, this.length),
                this[t] | (this[t + 1] << 8)
              );
            }),
          (f.prototype.readUint16BE = f.prototype.readUInt16BE =
            function (t, e) {
              return (
                (t >>>= 0),
                e || R(t, 2, this.length),
                (this[t] << 8) | this[t + 1]
              );
            }),
          (f.prototype.readUint32LE = f.prototype.readUInt32LE =
            function (t, e) {
              return (
                (t >>>= 0),
                e || R(t, 4, this.length),
                (this[t] | (this[t + 1] << 8) | (this[t + 2] << 16)) +
                  16777216 * this[t + 3]
              );
            }),
          (f.prototype.readUint32BE = f.prototype.readUInt32BE =
            function (t, e) {
              return (
                (t >>>= 0),
                e || R(t, 4, this.length),
                16777216 * this[t] +
                  ((this[t + 1] << 16) | (this[t + 2] << 8) | this[t + 3])
              );
            }),
          (f.prototype.readBigUInt64LE = X(function (t) {
            q((t >>>= 0), "offset");
            const e = this[t],
              r = this[t + 7];
            (void 0 !== e && void 0 !== r) || $(t, this.length - 8);
            const n =
                e + 256 * this[++t] + 65536 * this[++t] + this[++t] * 2 ** 24,
              o = this[++t] + 256 * this[++t] + 65536 * this[++t] + r * 2 ** 24;
            return BigInt(n) + (BigInt(o) << BigInt(32));
          })),
          (f.prototype.readBigUInt64BE = X(function (t) {
            q((t >>>= 0), "offset");
            const e = this[t],
              r = this[t + 7];
            (void 0 !== e && void 0 !== r) || $(t, this.length - 8);
            const n =
                e * 2 ** 24 + 65536 * this[++t] + 256 * this[++t] + this[++t],
              o = this[++t] * 2 ** 24 + 65536 * this[++t] + 256 * this[++t] + r;
            return (BigInt(n) << BigInt(32)) + BigInt(o);
          })),
          (f.prototype.readIntLE = function (t, e, r) {
            (t >>>= 0), (e >>>= 0), r || R(t, e, this.length);
            let n = this[t],
              o = 1,
              i = 0;
            for (; ++i < e && (o *= 256); ) n += this[t + i] * o;
            return (o *= 128), n >= o && (n -= Math.pow(2, 8 * e)), n;
          }),
          (f.prototype.readIntBE = function (t, e, r) {
            (t >>>= 0), (e >>>= 0), r || R(t, e, this.length);
            let n = e,
              o = 1,
              i = this[t + --n];
            for (; n > 0 && (o *= 256); ) i += this[t + --n] * o;
            return (o *= 128), i >= o && (i -= Math.pow(2, 8 * e)), i;
          }),
          (f.prototype.readInt8 = function (t, e) {
            return (
              (t >>>= 0),
              e || R(t, 1, this.length),
              128 & this[t] ? -1 * (255 - this[t] + 1) : this[t]
            );
          }),
          (f.prototype.readInt16LE = function (t, e) {
            (t >>>= 0), e || R(t, 2, this.length);
            const r = this[t] | (this[t + 1] << 8);
            return 32768 & r ? 4294901760 | r : r;
          }),
          (f.prototype.readInt16BE = function (t, e) {
            (t >>>= 0), e || R(t, 2, this.length);
            const r = this[t + 1] | (this[t] << 8);
            return 32768 & r ? 4294901760 | r : r;
          }),
          (f.prototype.readInt32LE = function (t, e) {
            return (
              (t >>>= 0),
              e || R(t, 4, this.length),
              this[t] |
                (this[t + 1] << 8) |
                (this[t + 2] << 16) |
                (this[t + 3] << 24)
            );
          }),
          (f.prototype.readInt32BE = function (t, e) {
            return (
              (t >>>= 0),
              e || R(t, 4, this.length),
              (this[t] << 24) |
                (this[t + 1] << 16) |
                (this[t + 2] << 8) |
                this[t + 3]
            );
          }),
          (f.prototype.readBigInt64LE = X(function (t) {
            q((t >>>= 0), "offset");
            const e = this[t],
              r = this[t + 7];
            (void 0 !== e && void 0 !== r) || $(t, this.length - 8);
            const n =
              this[t + 4] + 256 * this[t + 5] + 65536 * this[t + 6] + (r << 24);
            return (
              (BigInt(n) << BigInt(32)) +
              BigInt(
                e + 256 * this[++t] + 65536 * this[++t] + this[++t] * 2 ** 24
              )
            );
          })),
          (f.prototype.readBigInt64BE = X(function (t) {
            q((t >>>= 0), "offset");
            const e = this[t],
              r = this[t + 7];
            (void 0 !== e && void 0 !== r) || $(t, this.length - 8);
            const n =
              (e << 24) + 65536 * this[++t] + 256 * this[++t] + this[++t];
            return (
              (BigInt(n) << BigInt(32)) +
              BigInt(
                this[++t] * 2 ** 24 + 65536 * this[++t] + 256 * this[++t] + r
              )
            );
          })),
          (f.prototype.readFloatLE = function (t, e) {
            return (
              (t >>>= 0), e || R(t, 4, this.length), o.read(this, t, !0, 23, 4)
            );
          }),
          (f.prototype.readFloatBE = function (t, e) {
            return (
              (t >>>= 0), e || R(t, 4, this.length), o.read(this, t, !1, 23, 4)
            );
          }),
          (f.prototype.readDoubleLE = function (t, e) {
            return (
              (t >>>= 0), e || R(t, 8, this.length), o.read(this, t, !0, 52, 8)
            );
          }),
          (f.prototype.readDoubleBE = function (t, e) {
            return (
              (t >>>= 0), e || R(t, 8, this.length), o.read(this, t, !1, 52, 8)
            );
          }),
          (f.prototype.writeUintLE = f.prototype.writeUIntLE =
            function (t, e, r, n) {
              (t = +t),
                (e >>>= 0),
                (r >>>= 0),
                n || _(this, t, e, r, Math.pow(2, 8 * r) - 1, 0);
              let o = 1,
                i = 0;
              for (this[e] = 255 & t; ++i < r && (o *= 256); )
                this[e + i] = (t / o) & 255;
              return e + r;
            }),
          (f.prototype.writeUintBE = f.prototype.writeUIntBE =
            function (t, e, r, n) {
              (t = +t),
                (e >>>= 0),
                (r >>>= 0),
                n || _(this, t, e, r, Math.pow(2, 8 * r) - 1, 0);
              let o = r - 1,
                i = 1;
              for (this[e + o] = 255 & t; --o >= 0 && (i *= 256); )
                this[e + o] = (t / i) & 255;
              return e + r;
            }),
          (f.prototype.writeUint8 = f.prototype.writeUInt8 =
            function (t, e, r) {
              return (
                (t = +t),
                (e >>>= 0),
                r || _(this, t, e, 1, 255, 0),
                (this[e] = 255 & t),
                e + 1
              );
            }),
          (f.prototype.writeUint16LE = f.prototype.writeUInt16LE =
            function (t, e, r) {
              return (
                (t = +t),
                (e >>>= 0),
                r || _(this, t, e, 2, 65535, 0),
                (this[e] = 255 & t),
                (this[e + 1] = t >>> 8),
                e + 2
              );
            }),
          (f.prototype.writeUint16BE = f.prototype.writeUInt16BE =
            function (t, e, r) {
              return (
                (t = +t),
                (e >>>= 0),
                r || _(this, t, e, 2, 65535, 0),
                (this[e] = t >>> 8),
                (this[e + 1] = 255 & t),
                e + 2
              );
            }),
          (f.prototype.writeUint32LE = f.prototype.writeUInt32LE =
            function (t, e, r) {
              return (
                (t = +t),
                (e >>>= 0),
                r || _(this, t, e, 4, 4294967295, 0),
                (this[e + 3] = t >>> 24),
                (this[e + 2] = t >>> 16),
                (this[e + 1] = t >>> 8),
                (this[e] = 255 & t),
                e + 4
              );
            }),
          (f.prototype.writeUint32BE = f.prototype.writeUInt32BE =
            function (t, e, r) {
              return (
                (t = +t),
                (e >>>= 0),
                r || _(this, t, e, 4, 4294967295, 0),
                (this[e] = t >>> 24),
                (this[e + 1] = t >>> 16),
                (this[e + 2] = t >>> 8),
                (this[e + 3] = 255 & t),
                e + 4
              );
            }),
          (f.prototype.writeBigUInt64LE = X(function (t, e = 0) {
            return M(this, t, e, BigInt(0), BigInt("0xffffffffffffffff"));
          })),
          (f.prototype.writeBigUInt64BE = X(function (t, e = 0) {
            return C(this, t, e, BigInt(0), BigInt("0xffffffffffffffff"));
          })),
          (f.prototype.writeIntLE = function (t, e, r, n) {
            if (((t = +t), (e >>>= 0), !n)) {
              const n = Math.pow(2, 8 * r - 1);
              _(this, t, e, r, n - 1, -n);
            }
            let o = 0,
              i = 1,
              s = 0;
            for (this[e] = 255 & t; ++o < r && (i *= 256); )
              t < 0 && 0 === s && 0 !== this[e + o - 1] && (s = 1),
                (this[e + o] = (((t / i) >> 0) - s) & 255);
            return e + r;
          }),
          (f.prototype.writeIntBE = function (t, e, r, n) {
            if (((t = +t), (e >>>= 0), !n)) {
              const n = Math.pow(2, 8 * r - 1);
              _(this, t, e, r, n - 1, -n);
            }
            let o = r - 1,
              i = 1,
              s = 0;
            for (this[e + o] = 255 & t; --o >= 0 && (i *= 256); )
              t < 0 && 0 === s && 0 !== this[e + o + 1] && (s = 1),
                (this[e + o] = (((t / i) >> 0) - s) & 255);
            return e + r;
          }),
          (f.prototype.writeInt8 = function (t, e, r) {
            return (
              (t = +t),
              (e >>>= 0),
              r || _(this, t, e, 1, 127, -128),
              t < 0 && (t = 255 + t + 1),
              (this[e] = 255 & t),
              e + 1
            );
          }),
          (f.prototype.writeInt16LE = function (t, e, r) {
            return (
              (t = +t),
              (e >>>= 0),
              r || _(this, t, e, 2, 32767, -32768),
              (this[e] = 255 & t),
              (this[e + 1] = t >>> 8),
              e + 2
            );
          }),
          (f.prototype.writeInt16BE = function (t, e, r) {
            return (
              (t = +t),
              (e >>>= 0),
              r || _(this, t, e, 2, 32767, -32768),
              (this[e] = t >>> 8),
              (this[e + 1] = 255 & t),
              e + 2
            );
          }),
          (f.prototype.writeInt32LE = function (t, e, r) {
            return (
              (t = +t),
              (e >>>= 0),
              r || _(this, t, e, 4, 2147483647, -2147483648),
              (this[e] = 255 & t),
              (this[e + 1] = t >>> 8),
              (this[e + 2] = t >>> 16),
              (this[e + 3] = t >>> 24),
              e + 4
            );
          }),
          (f.prototype.writeInt32BE = function (t, e, r) {
            return (
              (t = +t),
              (e >>>= 0),
              r || _(this, t, e, 4, 2147483647, -2147483648),
              t < 0 && (t = 4294967295 + t + 1),
              (this[e] = t >>> 24),
              (this[e + 1] = t >>> 16),
              (this[e + 2] = t >>> 8),
              (this[e + 3] = 255 & t),
              e + 4
            );
          }),
          (f.prototype.writeBigInt64LE = X(function (t, e = 0) {
            return M(
              this,
              t,
              e,
              -BigInt("0x8000000000000000"),
              BigInt("0x7fffffffffffffff")
            );
          })),
          (f.prototype.writeBigInt64BE = X(function (t, e = 0) {
            return C(
              this,
              t,
              e,
              -BigInt("0x8000000000000000"),
              BigInt("0x7fffffffffffffff")
            );
          })),
          (f.prototype.writeFloatLE = function (t, e, r) {
            return N(this, t, e, !0, r);
          }),
          (f.prototype.writeFloatBE = function (t, e, r) {
            return N(this, t, e, !1, r);
          }),
          (f.prototype.writeDoubleLE = function (t, e, r) {
            return P(this, t, e, !0, r);
          }),
          (f.prototype.writeDoubleBE = function (t, e, r) {
            return P(this, t, e, !1, r);
          }),
          (f.prototype.copy = function (t, e, r, n) {
            if (!f.isBuffer(t))
              throw new TypeError("argument should be a Buffer");
            if (
              (r || (r = 0),
              n || 0 === n || (n = this.length),
              e >= t.length && (e = t.length),
              e || (e = 0),
              n > 0 && n < r && (n = r),
              n === r)
            )
              return 0;
            if (0 === t.length || 0 === this.length) return 0;
            if (e < 0) throw new RangeError("targetStart out of bounds");
            if (r < 0 || r >= this.length)
              throw new RangeError("Index out of range");
            if (n < 0) throw new RangeError("sourceEnd out of bounds");
            n > this.length && (n = this.length),
              t.length - e < n - r && (n = t.length - e + r);
            const o = n - r;
            return (
              this === t && "function" == typeof Uint8Array.prototype.copyWithin
                ? this.copyWithin(e, r, n)
                : Uint8Array.prototype.set.call(t, this.subarray(r, n), e),
              o
            );
          }),
          (f.prototype.fill = function (t, e, r, n) {
            if ("string" == typeof t) {
              if (
                ("string" == typeof e
                  ? ((n = e), (e = 0), (r = this.length))
                  : "string" == typeof r && ((n = r), (r = this.length)),
                void 0 !== n && "string" != typeof n)
              )
                throw new TypeError("encoding must be a string");
              if ("string" == typeof n && !f.isEncoding(n))
                throw new TypeError("Unknown encoding: " + n);
              if (1 === t.length) {
                const e = t.charCodeAt(0);
                (("utf8" === n && e < 128) || "latin1" === n) && (t = e);
              }
            } else
              "number" == typeof t
                ? (t &= 255)
                : "boolean" == typeof t && (t = Number(t));
            if (e < 0 || this.length < e || this.length < r)
              throw new RangeError("Out of range index");
            if (r <= e) return this;
            let o;
            if (
              ((e >>>= 0),
              (r = void 0 === r ? this.length : r >>> 0),
              t || (t = 0),
              "number" == typeof t)
            )
              for (o = e; o < r; ++o) this[o] = t;
            else {
              const i = f.isBuffer(t) ? t : f.from(t, n),
                s = i.length;
              if (0 === s)
                throw new TypeError(
                  'The value "' + t + '" is invalid for argument "value"'
                );
              for (o = 0; o < r - e; ++o) this[o + e] = i[o % s];
            }
            return this;
          });
        const F = {};
        function H(t, e, r) {
          F[t] = class extends r {
            constructor() {
              super(),
                Object.defineProperty(this, "message", {
                  value: e.apply(this, arguments),
                  writable: !0,
                  configurable: !0,
                }),
                (this.name = `${this.name} [${t}]`),
                this.stack,
                delete this.name;
            }
            get code() {
              return t;
            }
            set code(t) {
              Object.defineProperty(this, "code", {
                configurable: !0,
                enumerable: !0,
                value: t,
                writable: !0,
              });
            }
            toString() {
              return `${this.name} [${t}]: ${this.message}`;
            }
          };
        }
        function D(t) {
          let e = "",
            r = t.length;
          const n = "-" === t[0] ? 1 : 0;
          for (; r >= n + 4; r -= 3) e = `_${t.slice(r - 3, r)}${e}`;
          return `${t.slice(0, r)}${e}`;
        }
        function V(t, e, r, n, o, i) {
          if (t > r || t < e) {
            const n = "bigint" == typeof e ? "n" : "";
            let o;
            throw (
              ((o =
                i > 3
                  ? 0 === e || e === BigInt(0)
                    ? `>= 0${n} and < 2${n} ** ${8 * (i + 1)}${n}`
                    : `>= -(2${n} ** ${8 * (i + 1) - 1}${n}) and < 2 ** ${8 * (i + 1) - 1}${n}`
                  : `>= ${e}${n} and <= ${r}${n}`),
              new F.ERR_OUT_OF_RANGE("value", o, t))
            );
          }
          !(function (t, e, r) {
            q(e, "offset"),
              (void 0 !== t[e] && void 0 !== t[e + r]) ||
                $(e, t.length - (r + 1));
          })(n, o, i);
        }
        function q(t, e) {
          if ("number" != typeof t)
            throw new F.ERR_INVALID_ARG_TYPE(e, "number", t);
        }
        function $(t, e, r) {
          if (Math.floor(t) !== t)
            throw (
              (q(t, r), new F.ERR_OUT_OF_RANGE(r || "offset", "an integer", t))
            );
          if (e < 0) throw new F.ERR_BUFFER_OUT_OF_BOUNDS();
          throw new F.ERR_OUT_OF_RANGE(
            r || "offset",
            `>= ${r ? 1 : 0} and <= ${e}`,
            t
          );
        }
        H(
          "ERR_BUFFER_OUT_OF_BOUNDS",
          function (t) {
            return t
              ? `${t} is outside of buffer bounds`
              : "Attempt to access memory outside buffer bounds";
          },
          RangeError
        ),
          H(
            "ERR_INVALID_ARG_TYPE",
            function (t, e) {
              return `The "${t}" argument must be of type number. Received type ${typeof e}`;
            },
            TypeError
          ),
          H(
            "ERR_OUT_OF_RANGE",
            function (t, e, r) {
              let n = `The value of "${t}" is out of range.`,
                o = r;
              return (
                Number.isInteger(r) && Math.abs(r) > 2 ** 32
                  ? (o = D(String(r)))
                  : "bigint" == typeof r &&
                    ((o = String(r)),
                    (r > BigInt(2) ** BigInt(32) ||
                      r < -(BigInt(2) ** BigInt(32))) &&
                      (o = D(o)),
                    (o += "n")),
                (n += ` It must be ${e}. Received ${o}`),
                n
              );
            },
            RangeError
          );
        const K = /[^+/0-9A-Za-z-_]/g;
        function W(t, e) {
          let r;
          e = e || 1 / 0;
          const n = t.length;
          let o = null;
          const i = [];
          for (let s = 0; s < n; ++s) {
            if (((r = t.charCodeAt(s)), r > 55295 && r < 57344)) {
              if (!o) {
                if (r > 56319) {
                  (e -= 3) > -1 && i.push(239, 191, 189);
                  continue;
                }
                if (s + 1 === n) {
                  (e -= 3) > -1 && i.push(239, 191, 189);
                  continue;
                }
                o = r;
                continue;
              }
              if (r < 56320) {
                (e -= 3) > -1 && i.push(239, 191, 189), (o = r);
                continue;
              }
              r = 65536 + (((o - 55296) << 10) | (r - 56320));
            } else o && (e -= 3) > -1 && i.push(239, 191, 189);
            if (((o = null), r < 128)) {
              if ((e -= 1) < 0) break;
              i.push(r);
            } else if (r < 2048) {
              if ((e -= 2) < 0) break;
              i.push((r >> 6) | 192, (63 & r) | 128);
            } else if (r < 65536) {
              if ((e -= 3) < 0) break;
              i.push((r >> 12) | 224, ((r >> 6) & 63) | 128, (63 & r) | 128);
            } else {
              if (!(r < 1114112)) throw new Error("Invalid code point");
              if ((e -= 4) < 0) break;
              i.push(
                (r >> 18) | 240,
                ((r >> 12) & 63) | 128,
                ((r >> 6) & 63) | 128,
                (63 & r) | 128
              );
            }
          }
          return i;
        }
        function z(t) {
          return n.toByteArray(
            (function (t) {
              if ((t = (t = t.split("=")[0]).trim().replace(K, "")).length < 2)
                return "";
              for (; t.length % 4 != 0; ) t += "=";
              return t;
            })(t)
          );
        }
        function G(t, e, r, n) {
          let o;
          for (o = 0; o < n && !(o + r >= e.length || o >= t.length); ++o)
            e[o + r] = t[o];
          return o;
        }
        function Z(t, e) {
          return (
            t instanceof e ||
            (null != t &&
              null != t.constructor &&
              null != t.constructor.name &&
              t.constructor.name === e.name)
          );
        }
        function J(t) {
          return t != t;
        }
        const Y = (function () {
          const t = "0123456789abcdef",
            e = new Array(256);
          for (let r = 0; r < 16; ++r) {
            const n = 16 * r;
            for (let o = 0; o < 16; ++o) e[n + o] = t[r] + t[o];
          }
          return e;
        })();
        function X(t) {
          return "undefined" == typeof BigInt ? Q : t;
        }
        function Q() {
          throw new Error("BigInt not supported");
        }
      },
      251: (t, e) => {
        (e.read = function (t, e, r, n, o) {
          var i,
            s,
            a = 8 * o - n - 1,
            f = (1 << a) - 1,
            u = f >> 1,
            c = -7,
            l = r ? o - 1 : 0,
            h = r ? -1 : 1,
            d = t[e + l];
          for (
            l += h, i = d & ((1 << -c) - 1), d >>= -c, c += a;
            c > 0;
            i = 256 * i + t[e + l], l += h, c -= 8
          );
          for (
            s = i & ((1 << -c) - 1), i >>= -c, c += n;
            c > 0;
            s = 256 * s + t[e + l], l += h, c -= 8
          );
          if (0 === i) i = 1 - u;
          else {
            if (i === f) return s ? NaN : (1 / 0) * (d ? -1 : 1);
            (s += Math.pow(2, n)), (i -= u);
          }
          return (d ? -1 : 1) * s * Math.pow(2, i - n);
        }),
          (e.write = function (t, e, r, n, o, i) {
            var s,
              a,
              f,
              u = 8 * i - o - 1,
              c = (1 << u) - 1,
              l = c >> 1,
              h = 23 === o ? Math.pow(2, -24) - Math.pow(2, -77) : 0,
              d = n ? 0 : i - 1,
              p = n ? 1 : -1,
              y = e < 0 || (0 === e && 1 / e < 0) ? 1 : 0;
            for (
              e = Math.abs(e),
                isNaN(e) || e === 1 / 0
                  ? ((a = isNaN(e) ? 1 : 0), (s = c))
                  : ((s = Math.floor(Math.log(e) / Math.LN2)),
                    e * (f = Math.pow(2, -s)) < 1 && (s--, (f *= 2)),
                    (e += s + l >= 1 ? h / f : h * Math.pow(2, 1 - l)) * f >=
                      2 && (s++, (f /= 2)),
                    s + l >= c
                      ? ((a = 0), (s = c))
                      : s + l >= 1
                        ? ((a = (e * f - 1) * Math.pow(2, o)), (s += l))
                        : ((a = e * Math.pow(2, l - 1) * Math.pow(2, o)),
                          (s = 0)));
              o >= 8;
              t[r + d] = 255 & a, d += p, a /= 256, o -= 8
            );
            for (
              s = (s << o) | a, u += o;
              u > 0;
              t[r + d] = 255 & s, d += p, s /= 256, u -= 8
            );
            t[r + d - p] |= 128 * y;
          });
      },
      92: (t, e, r) => {
        "use strict";
        var n = (function () {
          if ("undefined" != typeof self) return self;
          if ("undefined" != typeof window) return window;
          if (void 0 !== r.g) return r.g;
          throw new Error("unable to locate global object");
        })();
        (t.exports = e = n.fetch),
          n.fetch && (e.default = n.fetch.bind(n)),
          (e.Headers = n.Headers),
          (e.Request = n.Request),
          (e.Response = n.Response);
      },
      374: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.mkCardanoWalletExtension = e.mkCip95Wallet = void 0);
        const n = r(493),
          o = r(329),
          i = r(772),
          s = r(353),
          a = r(329),
          f = r(493),
          u = r(287),
          c = new a.Encoder({ mapsAsObjects: !1, useRecords: !1 }),
          l = new a.Decoder({ mapsAsObjects: !1 });
        class h {
          constructor(t) {
            this.value = t;
          }
        }
        async function d(t, e) {
          const r = e.networkId ?? 0,
            o = t.addressBech32(r);
          console.debug("Wallet address:" + o);
          const i = u.Buffer.from(t.addressRawBytes(r)),
            a = i.toString("hex"),
            h = u.Buffer.from(t.rewardAddressRawBytes(r)).toString("hex"),
            d = u.Buffer.from(t.stakeKey.public).toString("hex");
          return {
            getBalance: async () => (await s.default.queryUtxos(o), "0"),
            submitTx: async (t) => (
              console.debug("[CardanoWallet] SubmittingTx: ", t),
              await s.default.submitTransaction(t).then(async (e) =>
                (function (t) {
                  let e = l.decode(u.Buffer.from(t, "hex"));
                  const r = Uint8Array.from(c.encode(e[0]));
                  return (0, f.blake2bHex)(r, void 0, 32);
                })(t)
              )
            ),
            getUtxos: (t, e) => p(o, i, t, e),
            getUsedAddresses: async () => [a],
            getUnusedAddresses: async () => [a],
            getChangeAddress: async () => a,
            getRewardAddresses: async () => [
              h,
              ...(e.extraRewardAddresses ?? []),
            ],
            getNetworkId: async () => r,
            experimental: {
              on: (t, e) => {},
              off: (t, e) => {},
              getCollateral: () => "",
            },
            cip95: {
              getPubDRepKey: async () => d,
              getUnregisteredPubStakeKeys: async () => [d],
              getRegisteredPubStakeKeys: async () => [
                d,
                ...(e.extraRegisteredPubStakeKeys ?? []),
              ],
            },
            getActivePubStakeKeys: async () => [h],
            signTx: async (r, o) => {
              let i = l.decode(u.Buffer.from(r, "hex"));
              const s = u.Buffer.from(c.encode(i)).toString("hex");
              r != s &&
                (console.warn("[CardanoWallet] Re-encoded tx is not same"),
                console.warn("[CardanoWallet]   Starting Tx", r),
                console.warn("[CardanoWallet] Re-Encoded Tx", s));
              const a = Uint8Array.from(c.encode(i[0])),
                f = n.blake2b(a, void 0, 32);
              console.debug(
                "[CardanoWallet] Signing Tx hash=" +
                  u.Buffer.from(f).toString("hex")
              );
              const h = await t.paymentKey.signRaw(f),
                d = new Map(),
                p = [[t.paymentKey.public, h]];
              if (e.enableStakeSigning) {
                console.debug("Signing stake key...");
                const e = await t.stakeKey.signRaw(f);
                p.push([t.stakeKey.public, e]);
              }
              return d.set(0, p), u.Buffer.from(c.encode(d)).toString("hex");
            },
            signData: async (t, e) => "",
            getExtensions: () => [{ cip: 95 }],
          };
        }
        (0, o.addExtension)({
          Class: h,
          tag: 259,
          encode: (t, e) => e(t.value),
          decode: (t) => new h(t),
        }),
          (e.mkCip95Wallet = d),
          (e.mkCardanoWalletExtension = async function () {
            let t = !1;
            return {
              apiVersion: "1.3.1",
              icon: "data:image/svg+xml,%3C%3Fxml version='1.0' encoding='utf-8'%3F%3E%3Csvg viewBox='0 0 500 500' xmlns='http://www.w3.org/2000/svg'%3E%3Crect x='309.36' y='12.441' width='121.115' height='472.347' style='fill: rgb(128  177  211)%3B'/%3E%3Cellipse style='fill: rgb(128  177  211)%3B' cx='231.272' cy='320.966' rx='171.791' ry='137.051'/%3E%3C/svg%3E",
              enable: async function () {
                const e = window.cardanoTestWallet.wallet,
                  r = window.cardanoTestWallet.config || {};
                if (null == e)
                  throw new Error(
                    "No wallet found. Please add a wallet via window.cardanoTestWallet.wallet first."
                  );
                return (t = !0), await d(i.ShelleyWallet.fromJson(e), r);
              },
              isEnabled: async function () {
                return t;
              },
              name: "Demos",
              supportedExtensions: [{ cip: 95 }],
            };
          });
        const p = async (t, e, r, n) => {
          function o(t) {
            const e = BigInt(t.lovelace);
            let r = new Map();
            for (let e in t) {
              const n = t[e];
              for (let t in n)
                if (r.has(e)) r.get(e).set(t, BigInt(n[t]));
                else {
                  let o = new Map();
                  o.set(t, BigInt(n[t])), r.set(e, o);
                }
            }
            return r.size > 0 ? [e, r] : e;
          }
          return (await s.default.queryUtxos(t)).map((t) => {
            const r = t.txin.split("#");
            return u.Buffer.from(
              c.encode([
                [u.Buffer.from(r[0], "hex"), BigInt(r[1])],
                [e, o(t.value)],
              ])
            ).toString("hex");
          });
        };
      },
      772: (t, e, r) => {
        "use strict";
        var n = r(287).Buffer;
        Object.defineProperty(e, "__esModule", { value: !0 }),
          (e.ShelleyWalletAddress = e.ShelleyWallet = e.Ed25519Key = void 0);
        const o = r(459),
          i = r(343),
          s = r(493);
        class a {
          constructor(t, e, r) {
            (this.private = t), (this.public = e), (this.pkh = r);
          }
          static async generate() {
            const t = o.ed25519.utils.randomPrivateKey();
            return await a.fromPrivateKey(t);
          }
          static async fromPrivateKey(t) {
            const e = o.ed25519.getPublicKey(t),
              r = s.blake2b(e, void 0, 28);
            return new a(t, e, r);
          }
          static async fromPrivateKeyHex(t) {
            return await a.fromPrivateKey(Uint8Array.from(n.from(t, "hex")));
          }
          bech32Pkh(t = "stake") {
            return i.bech32.encode(t, i.bech32.toWords(this.pkh));
          }
          bech32PublicKey(t = "vk_") {
            return i.bech32.encode(t, i.bech32.toWords(this.public));
          }
          bech32PrivateKey(t = "sk_") {
            return i.bech32.encode(t, i.bech32.toWords(this.private));
          }
          async signRaw(t) {
            return o.ed25519.sign(t, this.private);
          }
          async verify(t, e) {
            return o.ed25519.verify(e, t, this.public);
          }
          json() {
            return {
              private: n.from(this.private).toString("hex"),
              public: n.from(this.public).toString("hex"),
              pkh: n.from(this.pkh).toString("hex"),
            };
          }
          static fromJson(t) {
            if (!t || "object" != typeof t)
              throw new Error(
                "Invalid JSON format for Ed25519Key: Input must be a non-null object."
              );
            if (!t.private || !t.public || !t.pkh)
              throw new Error(
                "Invalid JSON format for Ed25519Key: Missing required fields (private, public, or pkh)."
              );
            return new a(
              Uint8Array.from(n.from(t.private, "hex")),
              Uint8Array.from(n.from(t.public, "hex")),
              Uint8Array.from(n.from(t.pkh, "hex"))
            );
          }
        }
        e.Ed25519Key = a;
        class f {
          constructor(t, e) {
            (this.paymentKey = t), (this.stakeKey = e);
          }
          static async generate() {
            return new f(await a.generate(), await a.generate());
          }
          addressBech32(t) {
            const e = 0 == t ? "addr_test" : "addr";
            return i.bech32.encode(
              e,
              i.bech32.toWords(n.from(this.addressRawBytes(t))),
              200
            );
          }
          addressRawBytes(t) {
            const e = new Uint8Array(57);
            return (
              (e[0] = t),
              e.set(this.paymentKey.pkh, 1),
              e.set(this.stakeKey.pkh, 29),
              e
            );
          }
          rewardAddressRawBytes(t) {
            const e = 224 | t,
              r = new Uint8Array(29);
            return (r[0] = e), r.set(this.stakeKey.pkh, 1), r;
          }
          rewardAddressBech32(t) {
            const e = 0 == t ? "stake_test" : "stake";
            return i.bech32.encode(
              e,
              i.bech32.toWords(n.from(this.rewardAddressRawBytes(t))),
              200
            );
          }
          json() {
            return {
              payment: this.paymentKey.json(),
              stake: this.stakeKey.json(),
            };
          }
          static fromJson(t) {
            if (!t || "object" != typeof t)
              throw new Error(
                "ShelleyWallet.fromJson: The input must be an object."
              );
            const e = t.payment,
              r = t.stake;
            if (!e || "object" != typeof e)
              throw new Error(
                "ShelleyWallet.fromJson : Invalid payment key: It must be an object."
              );
            if (!r || "object" != typeof r)
              throw new Error(
                "ShelleyWallet.fromJson : Invalid stake key: It must be an object."
              );
            return new f(a.fromJson(e), a.fromJson(r));
          }
          static dummy() {
            return f.fromJson({
              payment: {
                pkh: "595ac9bbf256bae584f56a4b671baa4b14a18c8098b8e571834bc12c",
                private:
                  "5a1380cd79ecaee48d66c14f7d92ddfc866490a3b59d44520e60f16309c8a17d",
                public:
                  "8d2f4d49118eb1156048b66dd6372cdb1f82da0f8e208d9f8ea4b388c79c09ad",
              },
              stake: {
                pkh: "6706efab75778c2f08b9a5321ead8bfc982a5c08b51a0b2a713cac52",
                private:
                  "24e8c012c7bef2f5823baef1c06dac253da860a43f0d1f43fc3c8349a4f719a1",
                public:
                  "f7a1eaea2691ee80b6c0d6f27482145d7037055829b1b26224a5d8f0c2243f16",
              },
            });
          }
        }
        e.ShelleyWallet = f;
        class u {
          constructor(t, e, r) {
            (this.network = "mainnet" == t ? 1 : "testnet" == t ? 0 : t),
              (this.paymentKeyHash = e),
              (this.stakeKeyHash = r);
          }
          static fromRawBytes(t) {
            let e;
            if (114 == t.length && "string" == typeof t) e = n.from(t, "hex");
            else {
              if (57 !== t.length)
                throw Error(
                  "ShelleyAddress.fromRawBytes: Invalid byte array length. expected: 57 got: " +
                    t.length
                );
              e = n.from(t);
            }
            let r = e.subarray(1, 29),
              o = e.subarray(29, 57);
            return new u(e.at(0), r, o);
          }
          toBech32() {
            const t = 0 == this.network ? "addr_test" : "addr";
            return i.bech32.encode(
              t,
              i.bech32.toWords(n.from(this.toRawBytes())),
              200
            );
          }
          toRawBytes() {
            const t = new Uint8Array(57);
            return (
              (t[0] = this.network),
              t.set(this.paymentKeyHash, 1),
              t.set(this.stakeKeyHash, 29),
              t
            );
          }
          toRawBytesHex() {
            return n.from(this.toRawBytes()).toString("hex");
          }
        }
        e.ShelleyWalletAddress = u;
      },
      353: (t, e, r) => {
        "use strict";
        Object.defineProperty(e, "__esModule", { value: !0 });
        const n = r(92),
          o = {
            apiUrl: window.cardanoTestWallet.kuberApiUrl,
            apiKey: window.cardanoTestWallet.kuberApiKey,
          },
          i = {
            submitTransaction: (t) =>
              (0, n.default)(o.apiUrl + "/api/v1/tx/submit", {
                method: "POST",
                headers: {
                  "Content-Type": "application/json",
                  "api-key": o.apiKey,
                },
                body: JSON.stringify({
                  tx: { description: "", type: "Tx ConwayEra", cborHex: t },
                }),
                redirect: "follow",
              }),
            queryUtxos: (t) =>
              (async function (t, e = "GET", r, i = "application/json") {
                const s = o.apiUrl + t,
                  a = { "api-key": o.apiKey };
                i && (a["content-type"] = i);
                const f = { method: e, headers: a };
                return (
                  "POST" === e && r && (f.body = r),
                  (0, n.default)(s, f).then(async (t) =>
                    200 === t.status
                      ? t.json()
                      : t.text().then((e) => {
                          let r, n;
                          try {
                            (n = JSON.parse(e)),
                              (r = n
                                ? Error(
                                    `KuberApi [Status ${t.status}] : ${n.message ? n.message : e}`
                                  )
                                : Error(
                                    `KuberApi [Status ${t.status}] : ${e}`
                                  ));
                          } catch (n) {
                            r = Error(`KuberApi [Status ${t.status}] : ${e}`);
                          }
                          throw ((r.status = t.status), r);
                        })
                  )
                );
              })("/api/v3/utxo?address=" + t),
          };
        e.default = i;
      },
      329: (t, e, r) => {
        "use strict";
        r.r(e),
          r.d(e, {
            ALWAYS: () => Vt,
            DECIMAL_FIT: () => $t,
            DECIMAL_ROUND: () => qt,
            Decoder: () => k,
            Encoder: () => It,
            FLOAT32_OPTIONS: () => at,
            NEVER: () => Dt,
            REUSE_BUFFER_MODE: () => Kt,
            Tag: () => q,
            addExtension: () => jt,
            clearSource: () => rt,
            decode: () => it,
            decodeIter: () => Jt,
            decodeMultiple: () => st,
            encode: () => Pt,
            encodeAsAsyncIterable: () => Ht,
            encodeAsIterable: () => Ft,
            encodeIter: () => Zt,
            isNativeAccelerationEnabled: () => N,
            roundFloat32: () => ft,
          });
        var n = r(287).Buffer;
        let o, i, s;
        try {
          o = new TextDecoder();
        } catch (t) {}
        let a = 0;
        const f = [],
          u = 57342,
          c = 57343,
          l = 57337,
          h = {};
        let d,
          p,
          y,
          g,
          b,
          w,
          m,
          E = f,
          B = 0,
          x = {},
          v = 0,
          A = 0,
          I = [],
          S = [],
          U = { useRecords: !1, mapsAsObjects: !0 },
          O = !1,
          T = 2;
        try {
          new Function("");
        } catch (t) {
          T = 1 / 0;
        }
        class k {
          constructor(t) {
            if (
              t &&
              ((!t.keyMap && !t._keyMap) ||
                t.useRecords ||
                ((t.useRecords = !1), (t.mapsAsObjects = !0)),
              !1 === t.useRecords &&
                void 0 === t.mapsAsObjects &&
                (t.mapsAsObjects = !0),
              t.getStructures && (t.getShared = t.getStructures),
              t.getShared &&
                !t.structures &&
                ((t.structures = []).uninitialized = !0),
              t.keyMap)
            ) {
              this.mapKey = new Map();
              for (let [e, r] of Object.entries(t.keyMap))
                this.mapKey.set(r, e);
            }
            Object.assign(this, t);
          }
          decodeKey(t) {
            return (this.keyMap && this.mapKey.get(t)) || t;
          }
          encodeKey(t) {
            return this.keyMap && this.keyMap.hasOwnProperty(t)
              ? this.keyMap[t]
              : t;
          }
          encodeKeys(t) {
            if (!this._keyMap) return t;
            let e = new Map();
            for (let [r, n] of Object.entries(t))
              e.set(this._keyMap.hasOwnProperty(r) ? this._keyMap[r] : r, n);
            return e;
          }
          decodeKeys(t) {
            if (!this._keyMap || "Map" != t.constructor.name) return t;
            if (!this._mapKey) {
              this._mapKey = new Map();
              for (let [t, e] of Object.entries(this._keyMap))
                this._mapKey.set(e, t);
            }
            let e = {};
            return (
              t.forEach(
                (t, r) =>
                  (e[C(this._mapKey.has(r) ? this._mapKey.get(r) : r)] = t)
              ),
              e
            );
          }
          mapDecode(t, e) {
            let r = this.decode(t);
            return this._keyMap && "Array" === r.constructor.name
              ? r.map((t) => this.decodeKeys(t))
              : r;
          }
          decode(t, e) {
            if (i)
              return et(
                () => (
                  rt(),
                  this ? this.decode(t, e) : k.prototype.decode.call(U, t, e)
                )
              );
            (s = e > -1 ? e : t.length),
              (a = 0),
              (B = 0),
              (A = 0),
              (p = null),
              (E = f),
              (y = null),
              (i = t);
            try {
              w =
                t.dataView ||
                (t.dataView = new DataView(
                  t.buffer,
                  t.byteOffset,
                  t.byteLength
                ));
            } catch (e) {
              if (((i = null), t instanceof Uint8Array)) throw e;
              throw new Error(
                "Source must be a Uint8Array or Buffer but was a " +
                  (t && "object" == typeof t ? t.constructor.name : typeof t)
              );
            }
            if (this instanceof k) {
              if (
                ((x = this),
                (b =
                  this.sharedValues &&
                  (this.pack
                    ? new Array(this.maxPrivatePackedValues || 16).concat(
                        this.sharedValues
                      )
                    : this.sharedValues)),
                this.structures)
              )
                return (d = this.structures), L();
              (!d || d.length > 0) && (d = []);
            } else (x = U), (!d || d.length > 0) && (d = []), (b = null);
            return L();
          }
          decodeMultiple(t, e) {
            let r,
              n = 0;
            try {
              let o = t.length;
              O = !0;
              let i = this ? this.decode(t, o) : ot.decode(t, o);
              if (!e) {
                for (r = [i]; a < o; ) (n = a), r.push(L());
                return r;
              }
              if (!1 === e(i)) return;
              for (; a < o; ) if (((n = a), !1 === e(L()))) return;
            } catch (t) {
              throw ((t.lastPosition = n), (t.values = r), t);
            } finally {
              (O = !1), rt();
            }
          }
        }
        function L() {
          try {
            let t = R();
            if (y) {
              if (a >= y.postBundlePosition) {
                let t = new Error("Unexpected bundle position");
                throw ((t.incomplete = !0), t);
              }
              (a = y.postBundlePosition), (y = null);
            }
            if (a == s) (d = null), (i = null), g && (g = null);
            else {
              if (a > s) {
                let t = new Error("Unexpected end of CBOR data");
                throw ((t.incomplete = !0), t);
              }
              if (!O)
                throw new Error("Data read, but end of buffer not reached");
            }
            return t;
          } catch (t) {
            throw (
              (rt(),
              (t instanceof RangeError ||
                t.message.startsWith("Unexpected end of buffer")) &&
                (t.incomplete = !0),
              t)
            );
          }
        }
        function R() {
          let t = i[a++],
            e = t >> 5;
          if (((t &= 31), t > 23))
            switch (t) {
              case 24:
                t = i[a++];
                break;
              case 25:
                if (7 == e)
                  return (function () {
                    let t = i[a++],
                      e = i[a++],
                      r = (127 & t) >> 2;
                    if (31 === r)
                      return e || 3 & t ? NaN : 128 & t ? -1 / 0 : 1 / 0;
                    if (0 === r) {
                      let r = (((3 & t) << 8) | e) / (1 << 24);
                      return 128 & t ? -r : r;
                    }
                    return (
                      (V[3] = (128 & t) | (56 + (r >> 1))),
                      (V[2] = ((7 & t) << 5) | (e >> 3)),
                      (V[1] = e << 5),
                      (V[0] = 0),
                      D[0]
                    );
                  })();
                (t = w.getUint16(a)), (a += 2);
                break;
              case 26:
                if (7 == e) {
                  let t = w.getFloat32(a);
                  if (x.useFloat32 > 2) {
                    let e = nt[((127 & i[a]) << 1) | (i[a + 1] >> 7)];
                    return (a += 4), ((e * t + (t > 0 ? 0.5 : -0.5)) >> 0) / e;
                  }
                  return (a += 4), t;
                }
                (t = w.getUint32(a)), (a += 4);
                break;
              case 27:
                if (7 == e) {
                  let t = w.getFloat64(a);
                  return (a += 8), t;
                }
                if (e > 1) {
                  if (w.getUint32(a) > 0)
                    throw new Error(
                      "JavaScript does not support arrays, maps, or strings with length over 4294967295"
                    );
                  t = w.getUint32(a + 4);
                } else
                  x.int64AsNumber
                    ? ((t = 4294967296 * w.getUint32(a)),
                      (t += w.getUint32(a + 4)))
                    : (t = w.getBigUint64(a));
                a += 8;
                break;
              case 31:
                switch (e) {
                  case 2:
                  case 3:
                    throw new Error(
                      "Indefinite length not supported for byte or text strings"
                    );
                  case 4:
                    let t,
                      r = [],
                      o = 0;
                    for (; (t = R()) != h; ) r[o++] = t;
                    return 4 == e ? r : 3 == e ? r.join("") : n.concat(r);
                  case 5:
                    let i;
                    if (x.mapsAsObjects) {
                      let t = {};
                      if (x.keyMap)
                        for (; (i = R()) != h; ) t[C(x.decodeKey(i))] = R();
                      else for (; (i = R()) != h; ) t[C(i)] = R();
                      return t;
                    }
                    {
                      m && ((x.mapsAsObjects = !0), (m = !1));
                      let t = new Map();
                      if (x.keyMap)
                        for (; (i = R()) != h; ) t.set(x.decodeKey(i), R());
                      else for (; (i = R()) != h; ) t.set(i, R());
                      return t;
                    }
                  case 7:
                    return h;
                  default:
                    throw new Error(
                      "Invalid major type for indefinite length " + e
                    );
                }
              default:
                throw new Error("Unknown token " + t);
            }
          switch (e) {
            case 0:
              return t;
            case 1:
              return ~t;
            case 2:
              return (
                (r = t),
                x.copyBuffers
                  ? Uint8Array.prototype.slice.call(i, a, (a += r))
                  : i.subarray(a, (a += r))
              );
            case 3:
              if (A >= a) return p.slice(a - v, (a += t) - v);
              if (0 == A && s < 140 && t < 32) {
                let e =
                  t < 16
                    ? H(t)
                    : (function (t) {
                        let e = a,
                          r = new Array(t);
                        for (let n = 0; n < t; n++) {
                          const t = i[a++];
                          if ((128 & t) > 0) return void (a = e);
                          r[n] = t;
                        }
                        return F.apply(String, r);
                      })(t);
                if (null != e) return e;
              }
              return j(t);
            case 4:
              let e = new Array(t);
              for (let r = 0; r < t; r++) e[r] = R();
              return e;
            case 5:
              if (x.mapsAsObjects) {
                let e = {};
                if (x.keyMap)
                  for (let r = 0; r < t; r++) e[C(x.decodeKey(R()))] = R();
                else for (let r = 0; r < t; r++) e[C(R())] = R();
                return e;
              }
              {
                m && ((x.mapsAsObjects = !0), (m = !1));
                let e = new Map();
                if (x.keyMap)
                  for (let r = 0; r < t; r++) e.set(x.decodeKey(R()), R());
                else for (let r = 0; r < t; r++) e.set(R(), R());
                return e;
              }
            case 6:
              if (t >= l) {
                let e = d[8191 & t];
                if (e) return e.read || (e.read = M(e)), e.read();
                if (t < 65536) {
                  if (t == c) {
                    let t = Q(),
                      e = R(),
                      r = R();
                    $(e, r);
                    let n = {};
                    if (x.keyMap)
                      for (let e = 2; e < t; e++)
                        n[C(x.decodeKey(r[e - 2]))] = R();
                    else for (let e = 2; e < t; e++) n[C(r[e - 2])] = R();
                    return n;
                  }
                  if (t == u) {
                    let t = Q(),
                      e = R();
                    for (let r = 2; r < t; r++) $(e++, R());
                    return R();
                  }
                  if (t == l)
                    return (function () {
                      let t = Q(),
                        e = a + R();
                      for (let e = 2; e < t; e++) {
                        let t = Q();
                        a += t;
                      }
                      let r = a;
                      return (
                        (a = e),
                        (y = [P(Q()), P(Q())]),
                        (y.position0 = 0),
                        (y.position1 = 0),
                        (y.postBundlePosition = a),
                        (a = r),
                        R()
                      );
                    })();
                  if (x.getShared && (tt(), (e = d[8191 & t]), e))
                    return e.read || (e.read = M(e)), e.read();
                }
              }
              let n = I[t];
              if (n) return n.handlesRead ? n(R) : n(R());
              {
                let e = R();
                for (let r = 0; r < S.length; r++) {
                  let n = S[r](t, e);
                  if (void 0 !== n) return n;
                }
                return new q(e, t);
              }
            case 7:
              switch (t) {
                case 20:
                  return !1;
                case 21:
                  return !0;
                case 22:
                  return null;
                case 23:
                  return;
                default:
                  let e = (b || G())[t];
                  if (void 0 !== e) return e;
                  throw new Error("Unknown token " + t);
              }
            default:
              if (isNaN(t)) {
                let t = new Error("Unexpected end of CBOR data");
                throw ((t.incomplete = !0), t);
              }
              throw new Error("Unknown CBOR token " + t);
          }
          var r;
        }
        const _ = /^[a-zA-Z_$][a-zA-Z\d_$]*$/;
        function M(t) {
          return (
            (t.slowReads = 0),
            function () {
              let t = i[a++];
              if (((t &= 31), t > 23))
                switch (t) {
                  case 24:
                    t = i[a++];
                    break;
                  case 25:
                    (t = w.getUint16(a)), (a += 2);
                    break;
                  case 26:
                    (t = w.getUint32(a)), (a += 4);
                    break;
                  default:
                    throw new Error(
                      "Expected array header, but got " + i[a - 1]
                    );
                }
              let e = this.compiledReader;
              for (; e; ) {
                if (e.propertyCount === t) return e(R);
                e = e.next;
              }
              if (this.slowReads++ >= T) {
                let r = this.length == t ? this : this.slice(0, t);
                return (
                  (e = x.keyMap
                    ? new Function(
                        "r",
                        "return {" +
                          r
                            .map((t) => x.decodeKey(t))
                            .map((t) =>
                              _.test(t)
                                ? C(t) + ":r()"
                                : "[" + JSON.stringify(t) + "]:r()"
                            )
                            .join(",") +
                          "}"
                      )
                    : new Function(
                        "r",
                        "return {" +
                          r
                            .map((t) =>
                              _.test(t)
                                ? C(t) + ":r()"
                                : "[" + JSON.stringify(t) + "]:r()"
                            )
                            .join(",") +
                          "}"
                      )),
                  this.compiledReader && (e.next = this.compiledReader),
                  (e.propertyCount = t),
                  (this.compiledReader = e),
                  e(R)
                );
              }
              let r = {};
              if (x.keyMap)
                for (let e = 0; e < t; e++) r[C(x.decodeKey(this[e]))] = R();
              else for (let e = 0; e < t; e++) r[C(this[e])] = R();
              return r;
            }
          );
        }
        function C(t) {
          if ("string" == typeof t) return "__proto__" === t ? "__proto_" : t;
          if ("object" != typeof t) return t.toString();
          throw new Error("Invalid property name type " + typeof t);
        }
        let j = P,
          N = !1;
        function P(t) {
          let e;
          if (t < 16 && (e = H(t))) return e;
          if (t > 64 && o) return o.decode(i.subarray(a, (a += t)));
          const r = a + t,
            n = [];
          for (e = ""; a < r; ) {
            const t = i[a++];
            if (0 == (128 & t)) n.push(t);
            else if (192 == (224 & t)) {
              const e = 63 & i[a++];
              n.push(((31 & t) << 6) | e);
            } else if (224 == (240 & t)) {
              const e = 63 & i[a++],
                r = 63 & i[a++];
              n.push(((31 & t) << 12) | (e << 6) | r);
            } else if (240 == (248 & t)) {
              let e =
                ((7 & t) << 18) |
                ((63 & i[a++]) << 12) |
                ((63 & i[a++]) << 6) |
                (63 & i[a++]);
              e > 65535 &&
                ((e -= 65536),
                n.push(((e >>> 10) & 1023) | 55296),
                (e = 56320 | (1023 & e))),
                n.push(e);
            } else n.push(t);
            n.length >= 4096 && ((e += F.apply(String, n)), (n.length = 0));
          }
          return n.length > 0 && (e += F.apply(String, n)), e;
        }
        let F = String.fromCharCode;
        function H(t) {
          if (t < 4) {
            if (t < 2) {
              if (0 === t) return "";
              {
                let t = i[a++];
                return (128 & t) > 1 ? void (a -= 1) : F(t);
              }
            }
            {
              let e = i[a++],
                r = i[a++];
              if ((128 & e) > 0 || (128 & r) > 0) return void (a -= 2);
              if (t < 3) return F(e, r);
              let n = i[a++];
              return (128 & n) > 0 ? void (a -= 3) : F(e, r, n);
            }
          }
          {
            let e = i[a++],
              r = i[a++],
              n = i[a++],
              o = i[a++];
            if (
              (128 & e) > 0 ||
              (128 & r) > 0 ||
              (128 & n) > 0 ||
              (128 & o) > 0
            )
              return void (a -= 4);
            if (t < 6) {
              if (4 === t) return F(e, r, n, o);
              {
                let t = i[a++];
                return (128 & t) > 0 ? void (a -= 5) : F(e, r, n, o, t);
              }
            }
            if (t < 8) {
              let s = i[a++],
                f = i[a++];
              if ((128 & s) > 0 || (128 & f) > 0) return void (a -= 6);
              if (t < 7) return F(e, r, n, o, s, f);
              let u = i[a++];
              return (128 & u) > 0 ? void (a -= 7) : F(e, r, n, o, s, f, u);
            }
            {
              let s = i[a++],
                f = i[a++],
                u = i[a++],
                c = i[a++];
              if (
                (128 & s) > 0 ||
                (128 & f) > 0 ||
                (128 & u) > 0 ||
                (128 & c) > 0
              )
                return void (a -= 8);
              if (t < 10) {
                if (8 === t) return F(e, r, n, o, s, f, u, c);
                {
                  let t = i[a++];
                  return (128 & t) > 0
                    ? void (a -= 9)
                    : F(e, r, n, o, s, f, u, c, t);
                }
              }
              if (t < 12) {
                let l = i[a++],
                  h = i[a++];
                if ((128 & l) > 0 || (128 & h) > 0) return void (a -= 10);
                if (t < 11) return F(e, r, n, o, s, f, u, c, l, h);
                let d = i[a++];
                return (128 & d) > 0
                  ? void (a -= 11)
                  : F(e, r, n, o, s, f, u, c, l, h, d);
              }
              {
                let l = i[a++],
                  h = i[a++],
                  d = i[a++],
                  p = i[a++];
                if (
                  (128 & l) > 0 ||
                  (128 & h) > 0 ||
                  (128 & d) > 0 ||
                  (128 & p) > 0
                )
                  return void (a -= 12);
                if (t < 14) {
                  if (12 === t) return F(e, r, n, o, s, f, u, c, l, h, d, p);
                  {
                    let t = i[a++];
                    return (128 & t) > 0
                      ? void (a -= 13)
                      : F(e, r, n, o, s, f, u, c, l, h, d, p, t);
                  }
                }
                {
                  let y = i[a++],
                    g = i[a++];
                  if ((128 & y) > 0 || (128 & g) > 0) return void (a -= 14);
                  if (t < 15)
                    return F(e, r, n, o, s, f, u, c, l, h, d, p, y, g);
                  let b = i[a++];
                  return (128 & b) > 0
                    ? void (a -= 15)
                    : F(e, r, n, o, s, f, u, c, l, h, d, p, y, g, b);
                }
              }
            }
          }
        }
        let D = new Float32Array(1),
          V = new Uint8Array(D.buffer, 0, 4);
        new Array(4096);
        class q {
          constructor(t, e) {
            (this.value = t), (this.tag = e);
          }
        }
        (I[0] = (t) => new Date(t)),
          (I[1] = (t) => new Date(Math.round(1e3 * t))),
          (I[2] = (t) => {
            let e = BigInt(0);
            for (let r = 0, n = t.byteLength; r < n; r++)
              e = (BigInt(t[r]) + e) << BigInt(8);
            return e;
          }),
          (I[3] = (t) => BigInt(-1) - I[2](t)),
          (I[4] = (t) => +(t[1] + "e" + t[0])),
          (I[5] = (t) => t[1] * Math.exp(t[0] * Math.log(2)));
        const $ = (t, e) => {
          let r = d[(t -= 57344)];
          r &&
            r.isShared &&
            ((d.restoreStructures || (d.restoreStructures = []))[t] = r),
            (d[t] = e),
            (e.read = M(e));
        };
        (I[105] = (t) => {
          let e = t.length,
            r = t[1];
          $(t[0], r);
          let n = {};
          for (let o = 2; o < e; o++) n[C(r[o - 2])] = t[o];
          return n;
        }),
          (I[14] = (t) =>
            y ? y[0].slice(y.position0, (y.position0 += t)) : new q(t, 14)),
          (I[15] = (t) =>
            y ? y[1].slice(y.position1, (y.position1 += t)) : new q(t, 15));
        let K = { Error, RegExp };
        I[27] = (t) => (K[t[0]] || Error)(t[1], t[2]);
        const W = (t) => {
          if (132 != i[a++]) {
            let t = new Error(
              "Packed values structure must be followed by a 4 element array"
            );
            throw (i.length < a && (t.incomplete = !0), t);
          }
          let e = t();
          if (!e || !e.length) {
            let t = new Error(
              "Packed values structure must be followed by a 4 element array"
            );
            throw ((t.incomplete = !0), t);
          }
          return (
            (b = b ? e.concat(b.slice(e.length)) : e),
            (b.prefixes = t()),
            (b.suffixes = t()),
            t()
          );
        };
        function z(t, e) {
          return "string" == typeof t
            ? t + e
            : t instanceof Array
              ? t.concat(e)
              : Object.assign({}, t, e);
        }
        function G() {
          if (!b) {
            if (!x.getShared) throw new Error("No packed values available");
            tt();
          }
          return b;
        }
        (W.handlesRead = !0),
          (I[51] = W),
          (I[6] = (t) => {
            if (!b) {
              if (!x.getShared) return new q(t, 6);
              tt();
            }
            if ("number" == typeof t)
              return b[16 + (t >= 0 ? 2 * t : -2 * t - 1)];
            let e = new Error(
              "No support for non-integer packed references yet"
            );
            throw (void 0 === t && (e.incomplete = !0), e);
          }),
          (I[28] = (t) => {
            g || ((g = new Map()), (g.id = 0));
            let e,
              r = g.id++;
            e = i[a] >> 5 == 4 ? [] : {};
            let n = { target: e };
            g.set(r, n);
            let o = t();
            return n.used ? Object.assign(e, o) : ((n.target = o), o);
          }),
          (I[28].handlesRead = !0),
          (I[29] = (t) => {
            let e = g.get(t);
            return (e.used = !0), e.target;
          }),
          (I[258] = (t) => new Set(t)),
          ((I[259] = (t) => (
            x.mapsAsObjects && ((x.mapsAsObjects = !1), (m = !0)), t()
          )).handlesRead = !0),
          S.push((t, e) =>
            t >= 225 && t <= 255
              ? z(G().prefixes[t - 224], e)
              : t >= 28704 && t <= 32767
                ? z(G().prefixes[t - 28672], e)
                : t >= 1879052288 && t <= 2147483647
                  ? z(G().prefixes[t - 1879048192], e)
                  : t >= 216 && t <= 223
                    ? z(e, G().suffixes[t - 216])
                    : t >= 27647 && t <= 28671
                      ? z(e, G().suffixes[t - 27639])
                      : t >= 1811940352 && t <= 1879048191
                        ? z(e, G().suffixes[t - 1811939328])
                        : 1399353956 == t
                          ? {
                              packedValues: b,
                              structures: d.slice(0),
                              version: e,
                            }
                          : 55799 == t
                            ? e
                            : void 0
          );
        const Z = 1 == new Uint8Array(new Uint16Array([1]).buffer)[0],
          J = [
            Uint8Array,
            Uint8ClampedArray,
            Uint16Array,
            Uint32Array,
            "undefined" == typeof BigUint64Array
              ? { name: "BigUint64Array" }
              : BigUint64Array,
            Int8Array,
            Int16Array,
            Int32Array,
            "undefined" == typeof BigInt64Array
              ? { name: "BigInt64Array" }
              : BigInt64Array,
            Float32Array,
            Float64Array,
          ],
          Y = [64, 68, 69, 70, 71, 72, 77, 78, 79, 85, 86];
        for (let t = 0; t < J.length; t++) X(J[t], Y[t]);
        function X(t, e) {
          let r,
            n = "get" + t.name.slice(0, -5);
          "function" == typeof t ? (r = t.BYTES_PER_ELEMENT) : (t = null);
          for (let o = 0; o < 2; o++) {
            if (!o && 1 == r) continue;
            let i = 2 == r ? 1 : 4 == r ? 2 : 3;
            I[o ? e : e - 4] =
              1 == r || o == Z
                ? (n) => {
                    if (!t)
                      throw new Error(
                        "Could not find typed array for code " + e
                      );
                    return x.copyBuffers ||
                      (1 !== r &&
                        (2 !== r || 1 & n.byteOffset) &&
                        (4 !== r || 3 & n.byteOffset) &&
                        (8 !== r || 7 & n.byteOffset))
                      ? new t(Uint8Array.prototype.slice.call(n, 0).buffer)
                      : new t(n.buffer, n.byteOffset, n.byteLength);
                  }
                : (r) => {
                    if (!t)
                      throw new Error(
                        "Could not find typed array for code " + e
                      );
                    let s = new DataView(r.buffer, r.byteOffset, r.byteLength),
                      a = r.length >> i,
                      f = new t(a),
                      u = s[n];
                    for (let t = 0; t < a; t++) f[t] = u.call(s, t << i, o);
                    return f;
                  };
          }
        }
        function Q() {
          let t = 31 & i[a++];
          if (t > 23)
            switch (t) {
              case 24:
                t = i[a++];
                break;
              case 25:
                (t = w.getUint16(a)), (a += 2);
                break;
              case 26:
                (t = w.getUint32(a)), (a += 4);
            }
          return t;
        }
        function tt() {
          if (x.getShared) {
            let t = et(() => ((i = null), x.getShared())) || {},
              e = t.structures || [];
            (x.sharedVersion = t.version),
              (b = x.sharedValues = t.packedValues),
              !0 === d
                ? (x.structures = d = e)
                : d.splice.apply(d, [0, e.length].concat(e));
          }
        }
        function et(t) {
          let e = s,
            r = a,
            n = B,
            o = v,
            f = A,
            u = p,
            c = E,
            l = g,
            h = y,
            b = new Uint8Array(i.slice(0, s)),
            m = d,
            I = x,
            S = O,
            U = t();
          return (
            (s = e),
            (a = r),
            (B = n),
            (v = o),
            (A = f),
            (p = u),
            (E = c),
            (g = l),
            (y = h),
            (i = b),
            (O = S),
            (d = m),
            (x = I),
            (w = new DataView(i.buffer, i.byteOffset, i.byteLength)),
            U
          );
        }
        function rt() {
          (i = null), (g = null), (d = null);
        }
        const nt = new Array(147);
        for (let t = 0; t < 256; t++)
          nt[t] = +("1e" + Math.floor(45.15 - 0.30103 * t));
        let ot = new k({ useRecords: !1 });
        const it = ot.decode,
          st = ot.decodeMultiple,
          at = { NEVER: 0, ALWAYS: 1, DECIMAL_ROUND: 3, DECIMAL_FIT: 4 };
        function ft(t) {
          D[0] = t;
          let e = nt[((127 & V[3]) << 1) | (V[2] >> 7)];
          return ((e * t + (t > 0 ? 0.5 : -0.5)) >> 0) / e;
        }
        let ut, ct, lt;
        try {
          ut = new TextEncoder();
        } catch (t) {}
        const ht = "object" == typeof globalThis && globalThis.Buffer,
          dt = void 0 !== ht,
          pt = dt ? ht.allocUnsafeSlow : Uint8Array,
          yt = dt ? ht : Uint8Array,
          gt = dt ? 4294967296 : 2144337920;
        let bt,
          wt,
          mt,
          Et,
          Bt = 0,
          xt = null;
        const vt = /[\u0080-\uFFFF]/,
          At = Symbol("record-id");
        class It extends k {
          constructor(t) {
            let e, r, n, o, i;
            super(t), (this.offset = 0), (t = t || {});
            let s = yt.prototype.utf8Write
                ? function (t, e, r) {
                    return wt.utf8Write(t, e, r);
                  }
                : !(!ut || !ut.encodeInto) &&
                  function (t, e) {
                    return ut.encodeInto(t, wt.subarray(e)).written;
                  },
              a = this,
              f = t.structures || t.saveStructures,
              u = t.maxSharedStructures;
            if ((null == u && (u = f ? 128 : 0), u > 8190))
              throw new Error("Maximum maxSharedStructure is 8190");
            let c = t.sequential;
            c && (u = 0),
              this.structures || (this.structures = []),
              this.saveStructures && (this.saveShared = this.saveStructures);
            let l,
              h,
              d,
              p = t.sharedValues;
            if (p) {
              d = Object.create(null);
              for (let t = 0, e = p.length; t < e; t++) d[p[t]] = t;
            }
            let y = [],
              g = 0,
              b = 0;
            (this.mapEncode = function (t, e) {
              return (
                this._keyMap &&
                  !this._mapped &&
                  "Array" === t.constructor.name &&
                  (t = t.map((t) => this.encodeKeys(t))),
                this.encode(t, e)
              );
            }),
              (this.encode = function (s, f) {
                if (
                  (wt ||
                    ((wt = new pt(8192)),
                    (mt = new DataView(wt.buffer, 0, 8192)),
                    (Bt = 0)),
                  (Et = wt.length - 10),
                  Et - Bt < 2048
                    ? ((wt = new pt(wt.length)),
                      (mt = new DataView(wt.buffer, 0, wt.length)),
                      (Et = wt.length - 10),
                      (Bt = 0))
                    : f === Kt && (Bt = (Bt + 7) & 2147483640),
                  (e = Bt),
                  a.useSelfDescribedHeader &&
                    (mt.setUint32(Bt, 3654940416), (Bt += 3)),
                  (i = a.structuredClone ? new Map() : null),
                  a.bundleStrings && "string" != typeof s
                    ? ((xt = []), (xt.size = 1 / 0))
                    : (xt = null),
                  (r = a.structures),
                  r)
                ) {
                  if (r.uninitialized) {
                    let t = a.getShared() || {};
                    (a.structures = r = t.structures || []),
                      (a.sharedVersion = t.version);
                    let e = (a.sharedValues = t.packedValues);
                    if (e) {
                      d = {};
                      for (let t = 0, r = e.length; t < r; t++) d[e[t]] = t;
                    }
                  }
                  let t = r.length;
                  if ((t > u && !c && (t = u), !r.transitions)) {
                    r.transitions = Object.create(null);
                    for (let e = 0; e < t; e++) {
                      let t = r[e];
                      if (!t) continue;
                      let n,
                        o = r.transitions;
                      for (let r = 0, i = t.length; r < i; r++) {
                        void 0 === o[At] && (o[At] = e);
                        let i = t[r];
                        (n = o[i]),
                          n || (n = o[i] = Object.create(null)),
                          (o = n);
                      }
                      o[At] = 1048576 | e;
                    }
                  }
                  c || (r.nextId = t);
                }
                if ((n && (n = !1), (o = r || []), (h = d), t.pack)) {
                  let e = new Map();
                  if (
                    ((e.values = []),
                    (e.encoder = a),
                    (e.maxValues =
                      t.maxPrivatePackedValues || (d ? 16 : 1 / 0)),
                    (e.objectMap = d || !1),
                    (e.samplingPackedValues = l),
                    Lt(s, e),
                    e.values.length > 0)
                  ) {
                    (wt[Bt++] = 216), (wt[Bt++] = 51), Ot(4);
                    let t = e.values;
                    w(t), Ot(0), Ot(0), (h = Object.create(d || null));
                    for (let e = 0, r = t.length; e < r; e++) h[t[e]] = e;
                  }
                }
                bt = f & zt;
                try {
                  if (bt) return;
                  if (
                    (w(s), xt && Ct(e, w), (a.offset = Bt), i && i.idsToInsert)
                  ) {
                    (Bt += 2 * i.idsToInsert.length),
                      Bt > Et && E(Bt),
                      (a.offset = Bt);
                    let t = (function (t, e) {
                      let r,
                        n = 2 * e.length,
                        o = t.length - n;
                      e.sort((t, e) => (t.offset > e.offset ? 1 : -1));
                      for (let r = 0; r < e.length; r++) {
                        let n = e[r];
                        n.id = r;
                        for (let e of n.references)
                          (t[e++] = r >> 8), (t[e] = 255 & r);
                      }
                      for (; (r = e.pop()); ) {
                        let e = r.offset;
                        t.copyWithin(e + n, e, o), (n -= 2);
                        let i = e + n;
                        (t[i++] = 216), (t[i++] = 28), (o = e);
                      }
                      return t;
                    })(wt.subarray(e, Bt), i.idsToInsert);
                    return (i = null), t;
                  }
                  return f & Kt
                    ? ((wt.start = e), (wt.end = Bt), wt)
                    : wt.subarray(e, Bt);
                } finally {
                  if (r)
                    if (
                      (b < 10 && b++, r.length > u && (r.length = u), g > 1e4)
                    )
                      (r.transitions = null),
                        (b = 0),
                        (g = 0),
                        y.length > 0 && (y = []);
                    else if (y.length > 0 && !c) {
                      for (let t = 0, e = y.length; t < e; t++)
                        y[t][At] = void 0;
                      y = [];
                    }
                  if (n && a.saveShared) {
                    a.structures.length > u &&
                      (a.structures = a.structures.slice(0, u));
                    let t = wt.subarray(e, Bt);
                    return !1 === a.updateSharedData() ? a.encode(s) : t;
                  }
                  f & Wt && (Bt = e);
                }
              }),
              (this.findCommonStringsToPack = () => (
                (l = new Map()),
                d || (d = Object.create(null)),
                (t) => {
                  let e = (t && t.threshold) || 4,
                    r = this.pack ? t.maxPrivatePackedValues || 16 : 0;
                  p || (p = this.sharedValues = []);
                  for (let [t, o] of l)
                    o.count > e && ((d[t] = r++), p.push(t), (n = !0));
                  for (; this.saveShared && !1 === this.updateSharedData(); );
                  l = null;
                }
              ));
            const w = (r) => {
                Bt > Et && (wt = E(Bt));
                var n,
                  o = typeof r;
                if ("string" === o) {
                  if (h) {
                    let e = h[r];
                    if (e >= 0)
                      return void (e < 16
                        ? (wt[Bt++] = e + 224)
                        : ((wt[Bt++] = 198),
                          w(1 & e ? (15 - e) >> 1 : (e - 16) >> 1)));
                    if (l && !t.pack) {
                      let t = l.get(r);
                      t ? t.count++ : l.set(r, { count: 1 });
                    }
                  }
                  let o,
                    i = r.length;
                  if (xt && i >= 4 && i < 1024) {
                    if ((xt.size += i) > 61440) {
                      let t,
                        r = (xt[0] ? 3 * xt[0].length + xt[1].length : 0) + 10;
                      Bt + r > Et && (wt = E(Bt + r)),
                        (wt[Bt++] = 217),
                        (wt[Bt++] = 223),
                        (wt[Bt++] = 249),
                        (wt[Bt++] = xt.position ? 132 : 130),
                        (wt[Bt++] = 26),
                        (t = Bt - e),
                        (Bt += 4),
                        xt.position && Ct(e, w),
                        (xt = ["", ""]),
                        (xt.size = 0),
                        (xt.position = t);
                    }
                    let t = vt.test(r);
                    return (
                      (xt[t ? 0 : 1] += r),
                      (wt[Bt++] = t ? 206 : 207),
                      void w(i)
                    );
                  }
                  o = i < 32 ? 1 : i < 256 ? 2 : i < 65536 ? 3 : 5;
                  let a = 3 * i;
                  if ((Bt + a > Et && (wt = E(Bt + a)), i < 64 || !s)) {
                    let t,
                      e,
                      s,
                      a = Bt + o;
                    for (t = 0; t < i; t++)
                      (e = r.charCodeAt(t)),
                        e < 128
                          ? (wt[a++] = e)
                          : e < 2048
                            ? ((wt[a++] = (e >> 6) | 192),
                              (wt[a++] = (63 & e) | 128))
                            : 55296 == (64512 & e) &&
                                56320 == (64512 & (s = r.charCodeAt(t + 1)))
                              ? ((e = 65536 + ((1023 & e) << 10) + (1023 & s)),
                                t++,
                                (wt[a++] = (e >> 18) | 240),
                                (wt[a++] = ((e >> 12) & 63) | 128),
                                (wt[a++] = ((e >> 6) & 63) | 128),
                                (wt[a++] = (63 & e) | 128))
                              : ((wt[a++] = (e >> 12) | 224),
                                (wt[a++] = ((e >> 6) & 63) | 128),
                                (wt[a++] = (63 & e) | 128));
                    n = a - Bt - o;
                  } else n = s(r, Bt + o, a);
                  n < 24
                    ? (wt[Bt++] = 96 | n)
                    : n < 256
                      ? (o < 2 && wt.copyWithin(Bt + 2, Bt + 1, Bt + 1 + n),
                        (wt[Bt++] = 120),
                        (wt[Bt++] = n))
                      : n < 65536
                        ? (o < 3 && wt.copyWithin(Bt + 3, Bt + 2, Bt + 2 + n),
                          (wt[Bt++] = 121),
                          (wt[Bt++] = n >> 8),
                          (wt[Bt++] = 255 & n))
                        : (o < 5 && wt.copyWithin(Bt + 5, Bt + 3, Bt + 3 + n),
                          (wt[Bt++] = 122),
                          mt.setUint32(Bt, n),
                          (Bt += 4)),
                    (Bt += n);
                } else if ("number" === o)
                  if (this.alwaysUseFloat || r >>> 0 !== r)
                    if (this.alwaysUseFloat || r >> 0 !== r) {
                      let t;
                      if (
                        (t = this.useFloat32) > 0 &&
                        r < 4294967296 &&
                        r >= -2147483648
                      ) {
                        let e;
                        if (
                          ((wt[Bt++] = 250),
                          mt.setFloat32(Bt, r),
                          t < 4 ||
                            (e =
                              r *
                              nt[((127 & wt[Bt]) << 1) | (wt[Bt + 1] >> 7)]) >>
                              0 ===
                              e)
                        )
                          return void (Bt += 4);
                        Bt--;
                      }
                      (wt[Bt++] = 251), mt.setFloat64(Bt, r), (Bt += 8);
                    } else
                      r >= -24
                        ? (wt[Bt++] = 31 - r)
                        : r >= -256
                          ? ((wt[Bt++] = 56), (wt[Bt++] = ~r))
                          : r >= -65536
                            ? ((wt[Bt++] = 57), mt.setUint16(Bt, ~r), (Bt += 2))
                            : ((wt[Bt++] = 58),
                              mt.setUint32(Bt, ~r),
                              (Bt += 4));
                  else
                    r < 24
                      ? (wt[Bt++] = r)
                      : r < 256
                        ? ((wt[Bt++] = 24), (wt[Bt++] = r))
                        : r < 65536
                          ? ((wt[Bt++] = 25),
                            (wt[Bt++] = r >> 8),
                            (wt[Bt++] = 255 & r))
                          : ((wt[Bt++] = 26), mt.setUint32(Bt, r), (Bt += 4));
                else if ("object" === o)
                  if (r) {
                    if (i) {
                      let t = i.get(r);
                      if (t) {
                        if (
                          ((wt[Bt++] = 216),
                          (wt[Bt++] = 29),
                          (wt[Bt++] = 25),
                          !t.references)
                        ) {
                          let e = i.idsToInsert || (i.idsToInsert = []);
                          (t.references = []), e.push(t);
                        }
                        return t.references.push(Bt - e), void (Bt += 2);
                      }
                      i.set(r, { offset: Bt - e });
                    }
                    let t = r.constructor;
                    if (t === Object) m(r, !0);
                    else if (t === Array) {
                      (n = r.length) < 24 ? (wt[Bt++] = 128 | n) : Ot(n);
                      for (let t = 0; t < n; t++) w(r[t]);
                    } else if (t === Map)
                      if (
                        ((this.mapsAsObjects
                          ? !1 !== this.useTag259ForMaps
                          : this.useTag259ForMaps) &&
                          ((wt[Bt++] = 217), (wt[Bt++] = 1), (wt[Bt++] = 3)),
                        (n = r.size) < 24
                          ? (wt[Bt++] = 160 | n)
                          : n < 256
                            ? ((wt[Bt++] = 184), (wt[Bt++] = n))
                            : n < 65536
                              ? ((wt[Bt++] = 185),
                                (wt[Bt++] = n >> 8),
                                (wt[Bt++] = 255 & n))
                              : ((wt[Bt++] = 186),
                                mt.setUint32(Bt, n),
                                (Bt += 4)),
                        a.keyMap)
                      )
                        for (let [t, e] of r) w(a.encodeKey(t)), w(e);
                      else for (let [t, e] of r) w(t), w(e);
                    else {
                      for (let t = 0, e = ct.length; t < e; t++)
                        if (r instanceof lt[t]) {
                          let e = ct[t],
                            n = e.tag;
                          return (
                            null == n &&
                              (n = e.getTag && e.getTag.call(this, r)),
                            n < 24
                              ? (wt[Bt++] = 192 | n)
                              : n < 256
                                ? ((wt[Bt++] = 216), (wt[Bt++] = n))
                                : n < 65536
                                  ? ((wt[Bt++] = 217),
                                    (wt[Bt++] = n >> 8),
                                    (wt[Bt++] = 255 & n))
                                  : n > -1 &&
                                    ((wt[Bt++] = 218),
                                    mt.setUint32(Bt, n),
                                    (Bt += 4)),
                            void e.encode.call(this, r, w, E)
                          );
                        }
                      if (r[Symbol.iterator]) {
                        if (bt) {
                          let t = new Error(
                            "Iterable should be serialized as iterator"
                          );
                          throw ((t.iteratorNotHandled = !0), t);
                        }
                        wt[Bt++] = 159;
                        for (let t of r) w(t);
                        return void (wt[Bt++] = 255);
                      }
                      if (r[Symbol.asyncIterator] || kt(r)) {
                        let t = new Error(
                          "Iterable/blob should be serialized as iterator"
                        );
                        throw ((t.iteratorNotHandled = !0), t);
                      }
                      if (this.useToJSON && r.toJSON) {
                        const t = r.toJSON();
                        if (t !== r) return w(t);
                      }
                      m(r, !r.hasOwnProperty);
                    }
                  } else wt[Bt++] = 246;
                else if ("boolean" === o) wt[Bt++] = r ? 245 : 244;
                else if ("bigint" === o) {
                  if (r < BigInt(1) << BigInt(64) && r >= 0)
                    (wt[Bt++] = 27), mt.setBigUint64(Bt, r);
                  else if (r > -(BigInt(1) << BigInt(64)) && r < 0)
                    (wt[Bt++] = 59), mt.setBigUint64(Bt, -r - BigInt(1));
                  else {
                    if (!this.largeBigIntToFloat)
                      throw new RangeError(
                        r +
                          " was too large to fit in CBOR 64-bit integer format, set largeBigIntToFloat to convert to float-64"
                      );
                    (wt[Bt++] = 251), mt.setFloat64(Bt, Number(r));
                  }
                  Bt += 8;
                } else {
                  if ("undefined" !== o) throw new Error("Unknown type: " + o);
                  wt[Bt++] = 247;
                }
              },
              m =
                !1 === this.useRecords
                  ? this.variableMapSize
                    ? (t) => {
                        let e = Object.keys(t),
                          r = Object.values(t),
                          n = e.length;
                        if (
                          (n < 24
                            ? (wt[Bt++] = 160 | n)
                            : n < 256
                              ? ((wt[Bt++] = 184), (wt[Bt++] = n))
                              : n < 65536
                                ? ((wt[Bt++] = 185),
                                  (wt[Bt++] = n >> 8),
                                  (wt[Bt++] = 255 & n))
                                : ((wt[Bt++] = 186),
                                  mt.setUint32(Bt, n),
                                  (Bt += 4)),
                          a.keyMap)
                        )
                          for (let t = 0; t < n; t++)
                            w(a.encodeKey(e[t])), w(r[t]);
                        else for (let t = 0; t < n; t++) w(e[t]), w(r[t]);
                      }
                    : (t, r) => {
                        wt[Bt++] = 185;
                        let n = Bt - e;
                        Bt += 2;
                        let o = 0;
                        if (a.keyMap)
                          for (let e in t)
                            (r || t.hasOwnProperty(e)) &&
                              (w(a.encodeKey(e)), w(t[e]), o++);
                        else
                          for (let e in t)
                            (r || t.hasOwnProperty(e)) && (w(e), w(t[e]), o++);
                        (wt[n++ + e] = o >> 8), (wt[n + e] = 255 & o);
                      }
                  : (t, e) => {
                      let r,
                        i,
                        s,
                        a =
                          o.transitions ||
                          (o.transitions = Object.create(null)),
                        f = 0,
                        c = 0;
                      if (this.keyMap) {
                        (s = Object.keys(t).map((t) => this.encodeKey(t))),
                          (c = s.length);
                        for (let t = 0; t < c; t++) {
                          let e = s[t];
                          (r = a[e]),
                            r || ((r = a[e] = Object.create(null)), f++),
                            (a = r);
                        }
                      } else
                        for (let n in t)
                          (e || t.hasOwnProperty(n)) &&
                            ((r = a[n]),
                            r ||
                              (1048576 & a[At] && (i = 65535 & a[At]),
                              (r = a[n] = Object.create(null)),
                              f++),
                            (a = r),
                            c++);
                      let l = a[At];
                      if (void 0 !== l)
                        (l &= 65535),
                          (wt[Bt++] = 217),
                          (wt[Bt++] = (l >> 8) | 224),
                          (wt[Bt++] = 255 & l);
                      else {
                        if (
                          (s ||
                            (s = a.__keys__ || (a.__keys__ = Object.keys(t))),
                          void 0 === i
                            ? ((l = o.nextId++),
                              l || ((l = 0), (o.nextId = 1)),
                              l >= 256 && (o.nextId = (l = u) + 1))
                            : (l = i),
                          (o[l] = s),
                          !(l < u))
                        ) {
                          if (
                            ((a[At] = l),
                            mt.setUint32(Bt, 3655335680),
                            (Bt += 3),
                            f && (g += b * f),
                            y.length >= 256 - u && (y.shift()[At] = void 0),
                            y.push(a),
                            Ot(c + 2),
                            w(57344 + l),
                            w(s),
                            null === e)
                          )
                            return;
                          for (let r in t)
                            (e || t.hasOwnProperty(r)) && w(t[r]);
                          return;
                        }
                        (wt[Bt++] = 217),
                          (wt[Bt++] = (l >> 8) | 224),
                          (wt[Bt++] = 255 & l),
                          (a = o.transitions);
                        for (let t = 0; t < c; t++)
                          (void 0 === a[At] || 1048576 & a[At]) && (a[At] = l),
                            (a = a[s[t]]);
                        (a[At] = 1048576 | l), (n = !0);
                      }
                      if ((c < 24 ? (wt[Bt++] = 128 | c) : Ot(c), null !== e))
                        for (let r in t) (e || t.hasOwnProperty(r)) && w(t[r]);
                    },
              E = (t) => {
                let r;
                if (t > 16777216) {
                  if (t - e > gt)
                    throw new Error(
                      "Encoded buffer would be larger than maximum buffer size"
                    );
                  r = Math.min(
                    gt,
                    4096 *
                      Math.round(
                        Math.max((t - e) * (t > 67108864 ? 1.25 : 2), 4194304) /
                          4096
                      )
                  );
                } else
                  r = (1 + (Math.max((t - e) << 2, wt.length - 1) >> 12)) << 12;
                let n = new pt(r);
                return (
                  (mt = new DataView(n.buffer, 0, r)),
                  wt.copy ? wt.copy(n, 0, e, t) : n.set(wt.slice(e, t)),
                  (Bt -= e),
                  (e = 0),
                  (Et = n.length - 10),
                  (wt = n)
                );
              };
            let B = 100,
              x = 1e3;
            function* v(t, r, n) {
              let o = t.constructor;
              if (o === Object) {
                let e = !1 !== a.useRecords;
                e ? m(t, null) : St(Object.keys(t).length, 160);
                for (let n in t) {
                  let o = t[n];
                  e || w(n),
                    o && "object" == typeof o
                      ? r[n]
                        ? yield* v(o, r[n])
                        : yield* A(o, r, n)
                      : w(o);
                }
              } else if (o === Array) {
                let n = t.length;
                Ot(n);
                for (let o = 0; o < n; o++) {
                  let n = t[o];
                  n && ("object" == typeof n || Bt - e > B)
                    ? r.element
                      ? yield* v(n, r.element)
                      : yield* A(n, r, "element")
                    : w(n);
                }
              } else if (t[Symbol.iterator]) {
                wt[Bt++] = 159;
                for (let n of t)
                  n && ("object" == typeof n || Bt - e > B)
                    ? r.element
                      ? yield* v(n, r.element)
                      : yield* A(n, r, "element")
                    : w(n);
                wt[Bt++] = 255;
              } else
                kt(t)
                  ? (St(t.size, 64), yield wt.subarray(e, Bt), yield t, I())
                  : t[Symbol.asyncIterator]
                    ? ((wt[Bt++] = 159),
                      yield wt.subarray(e, Bt),
                      yield t,
                      I(),
                      (wt[Bt++] = 255))
                    : w(t);
              n && Bt > e
                ? yield wt.subarray(e, Bt)
                : Bt - e > B && (yield wt.subarray(e, Bt), I());
            }
            function* A(t, r, n) {
              let o = Bt - e;
              try {
                w(t), Bt - e > B && (yield wt.subarray(e, Bt), I());
              } catch (i) {
                if (!i.iteratorNotHandled) throw i;
                (r[n] = {}), (Bt = e + o), yield* v.call(this, t, r[n]);
              }
            }
            function I() {
              (B = x), a.encode(null, zt);
            }
            function S(t, e, r) {
              return (
                (B = e && e.chunkThreshold ? (x = e.chunkThreshold) : 100),
                t && "object" == typeof t
                  ? (a.encode(null, zt),
                    r(t, a.iterateProperties || (a.iterateProperties = {}), !0))
                  : [a.encode(t)]
              );
            }
            async function* U(t, e) {
              for (let r of v(t, e, !0)) {
                let t = r.constructor;
                if (t === yt || t === Uint8Array) yield r;
                else if (kt(r)) {
                  let t,
                    e = r.stream().getReader();
                  for (; !(t = await e.read()).done; ) yield t.value;
                } else if (r[Symbol.asyncIterator])
                  for await (let t of r)
                    I(),
                      t
                        ? yield* U(t, e.async || (e.async = {}))
                        : yield a.encode(t);
                else yield r;
              }
            }
            (this.encodeAsIterable = function (t, e) {
              return S(t, e, v);
            }),
              (this.encodeAsAsyncIterable = function (t, e) {
                return S(t, e, U);
              });
          }
          useBuffer(t) {
            (wt = t),
              (mt = new DataView(wt.buffer, wt.byteOffset, wt.byteLength)),
              (Bt = 0);
          }
          clearSharedData() {
            this.structures && (this.structures = []),
              this.sharedValues && (this.sharedValues = void 0);
          }
          updateSharedData() {
            let t = this.sharedVersion || 0;
            this.sharedVersion = t + 1;
            let e = this.structures.slice(0),
              r = new Ut(e, this.sharedValues, this.sharedVersion),
              n = this.saveShared(r, (e) => ((e && e.version) || 0) == t);
            return (
              !1 === n
                ? ((r = this.getShared() || {}),
                  (this.structures = r.structures || []),
                  (this.sharedValues = r.packedValues),
                  (this.sharedVersion = r.version),
                  (this.structures.nextId = this.structures.length))
                : e.forEach((t, e) => (this.structures[e] = t)),
              n
            );
          }
        }
        function St(t, e) {
          t < 24
            ? (wt[Bt++] = e | t)
            : t < 256
              ? ((wt[Bt++] = 24 | e), (wt[Bt++] = t))
              : t < 65536
                ? ((wt[Bt++] = 25 | e),
                  (wt[Bt++] = t >> 8),
                  (wt[Bt++] = 255 & t))
                : ((wt[Bt++] = 26 | e), mt.setUint32(Bt, t), (Bt += 4));
        }
        class Ut {
          constructor(t, e, r) {
            (this.structures = t), (this.packedValues = e), (this.version = r);
          }
        }
        function Ot(t) {
          t < 24
            ? (wt[Bt++] = 128 | t)
            : t < 256
              ? ((wt[Bt++] = 152), (wt[Bt++] = t))
              : t < 65536
                ? ((wt[Bt++] = 153), (wt[Bt++] = t >> 8), (wt[Bt++] = 255 & t))
                : ((wt[Bt++] = 154), mt.setUint32(Bt, t), (Bt += 4));
        }
        const Tt = "undefined" == typeof Blob ? function () {} : Blob;
        function kt(t) {
          if (t instanceof Tt) return !0;
          let e = t[Symbol.toStringTag];
          return "Blob" === e || "File" === e;
        }
        function Lt(t, e) {
          switch (typeof t) {
            case "string":
              if (t.length > 3) {
                if (e.objectMap[t] > -1 || e.values.length >= e.maxValues)
                  return;
                let r = e.get(t);
                if (r) 2 == ++r.count && e.values.push(t);
                else if ((e.set(t, { count: 1 }), e.samplingPackedValues)) {
                  let r = e.samplingPackedValues.get(t);
                  r ? r.count++ : e.samplingPackedValues.set(t, { count: 1 });
                }
              }
              break;
            case "object":
              if (t)
                if (t instanceof Array)
                  for (let r = 0, n = t.length; r < n; r++) Lt(t[r], e);
                else {
                  let n = !e.encoder.useRecords;
                  for (var r in t)
                    t.hasOwnProperty(r) && (n && Lt(r, e), Lt(t[r], e));
                }
              break;
            case "function":
              console.log(t);
          }
        }
        const Rt = 1 == new Uint8Array(new Uint16Array([1]).buffer)[0];
        function _t(t, e) {
          return (
            !Rt && e > 1 && (t -= 4),
            {
              tag: t,
              encode: function (t, e) {
                let r = t.byteLength,
                  n = t.byteOffset || 0,
                  o = t.buffer || t;
                e(dt ? ht.from(o, n, r) : new Uint8Array(o, n, r));
              },
            }
          );
        }
        function Mt(t, e) {
          let r = t.byteLength;
          r < 24
            ? (wt[Bt++] = 64 + r)
            : r < 256
              ? ((wt[Bt++] = 88), (wt[Bt++] = r))
              : r < 65536
                ? ((wt[Bt++] = 89), (wt[Bt++] = r >> 8), (wt[Bt++] = 255 & r))
                : ((wt[Bt++] = 90), mt.setUint32(Bt, r), (Bt += 4)),
            Bt + r >= wt.length && e(Bt + r),
            wt.set(t.buffer ? t : new Uint8Array(t), Bt),
            (Bt += r);
        }
        function Ct(t, e) {
          mt.setUint32(xt.position + t, Bt - xt.position - t + 1);
          let r = xt;
          (xt = null), e(r[0]), e(r[1]);
        }
        function jt(t) {
          if (t.Class) {
            if (!t.encode) throw new Error("Extension has no encode function");
            lt.unshift(t.Class), ct.unshift(t);
          }
          !(function (t) {
            I[t.tag] = t.decode;
          })(t);
        }
        (lt = [
          Date,
          Set,
          Error,
          RegExp,
          q,
          ArrayBuffer,
          Uint8Array,
          Uint8ClampedArray,
          Uint16Array,
          Uint32Array,
          "undefined" == typeof BigUint64Array
            ? function () {}
            : BigUint64Array,
          Int8Array,
          Int16Array,
          Int32Array,
          "undefined" == typeof BigInt64Array ? function () {} : BigInt64Array,
          Float32Array,
          Float64Array,
          Ut,
        ]),
          (ct = [
            {
              tag: 1,
              encode(t, e) {
                let r = t.getTime() / 1e3;
                (this.useTimestamp32 || 0 === t.getMilliseconds()) &&
                r >= 0 &&
                r < 4294967296
                  ? ((wt[Bt++] = 26), mt.setUint32(Bt, r), (Bt += 4))
                  : ((wt[Bt++] = 251), mt.setFloat64(Bt, r), (Bt += 8));
              },
            },
            {
              tag: 258,
              encode(t, e) {
                e(Array.from(t));
              },
            },
            {
              tag: 27,
              encode(t, e) {
                e([t.name, t.message]);
              },
            },
            {
              tag: 27,
              encode(t, e) {
                e(["RegExp", t.source, t.flags]);
              },
            },
            {
              getTag: (t) => t.tag,
              encode(t, e) {
                e(t.value);
              },
            },
            {
              encode(t, e, r) {
                Mt(t, r);
              },
            },
            {
              getTag(t) {
                if (
                  t.constructor === Uint8Array &&
                  (this.tagUint8Array || (dt && !1 !== this.tagUint8Array))
                )
                  return 64;
              },
              encode(t, e, r) {
                Mt(t, r);
              },
            },
            _t(68, 1),
            _t(69, 2),
            _t(70, 4),
            _t(71, 8),
            _t(72, 1),
            _t(77, 2),
            _t(78, 4),
            _t(79, 8),
            _t(85, 4),
            _t(86, 8),
            {
              encode(t, e) {
                let r = t.packedValues || [],
                  n = t.structures || [];
                if (r.values.length > 0) {
                  (wt[Bt++] = 216), (wt[Bt++] = 51), Ot(4);
                  let t = r.values;
                  e(t),
                    Ot(0),
                    Ot(0),
                    (packedObjectMap = Object.create(
                      sharedPackedObjectMap || null
                    ));
                  for (let e = 0, r = t.length; e < r; e++)
                    packedObjectMap[t[e]] = e;
                }
                if (n) {
                  mt.setUint32(Bt, 3655335424), (Bt += 3);
                  let r = n.slice(0);
                  r.unshift(57344), r.push(new q(t.version, 1399353956)), e(r);
                } else e(new q(t.version, 1399353956));
              },
            },
          ]);
        let Nt = new It({ useRecords: !1 });
        const Pt = Nt.encode,
          Ft = Nt.encodeAsIterable,
          Ht = Nt.encodeAsAsyncIterable,
          { NEVER: Dt, ALWAYS: Vt, DECIMAL_ROUND: qt, DECIMAL_FIT: $t } = at,
          Kt = 512,
          Wt = 1024,
          zt = 2048;
        var Gt = r(287).Buffer;
        function Zt(t, e = {}) {
          if (t && "object" == typeof t) {
            if ("function" == typeof t[Symbol.iterator])
              return (function* (t, e) {
                const r = new It(e);
                for (const e of t) yield r.encode(e);
              })(t, e);
            if (
              "function" == typeof t.then ||
              "function" == typeof t[Symbol.asyncIterator]
            )
              return (async function* (t, e) {
                const r = new It(e);
                for await (const e of t) yield r.encode(e);
              })(t, e);
            throw new Error(
              "first argument must be an Iterable, Async Iterable, Iterator, Async Iterator, or a Promise"
            );
          }
          throw new Error(
            "first argument must be an Iterable, Async Iterable, or a Promise for an Async Iterable"
          );
        }
        function Jt(t, e = {}) {
          if (!t || "object" != typeof t)
            throw new Error(
              "first argument must be an Iterable, Async Iterable, Iterator, Async Iterator, or a promise"
            );
          const r = new k(e);
          let n;
          const o = (t) => {
            let e;
            n && ((t = Gt.concat([n, t])), (n = void 0));
            try {
              e = r.decodeMultiple(t);
            } catch (r) {
              if (!r.incomplete) throw r;
              (n = t.slice(r.lastPosition)), (e = r.values);
            }
            return e;
          };
          return "function" == typeof t[Symbol.iterator]
            ? (function* () {
                for (const e of t) yield* o(e);
              })()
            : "function" == typeof t[Symbol.asyncIterator]
              ? (async function* () {
                  for await (const e of t) yield* o(e);
                })()
              : void 0;
        }
      },
    },
    e = {};
  function r(n) {
    var o = e[n];
    if (void 0 !== o) return o.exports;
    var i = (e[n] = { exports: {} });
    return t[n](i, i.exports, r), i.exports;
  }
  (r.d = (t, e) => {
    for (var n in e)
      r.o(e, n) &&
        !r.o(t, n) &&
        Object.defineProperty(t, n, { enumerable: !0, get: e[n] });
  }),
    (r.g = (function () {
      if ("object" == typeof globalThis) return globalThis;
      try {
        return this || new Function("return this")();
      } catch (t) {
        if ("object" == typeof window) return window;
      }
    })()),
    (r.o = (t, e) => Object.prototype.hasOwnProperty.call(t, e)),
    (r.r = (t) => {
      "undefined" != typeof Symbol &&
        Symbol.toStringTag &&
        Object.defineProperty(t, Symbol.toStringTag, { value: "Module" }),
        Object.defineProperty(t, "__esModule", { value: !0 });
    }),
    (() => {
      "use strict";
      const t = r(374);
      (async () => {
        if (null == window.cardanoTestWallet.walletName)
          throw new Error(
            'Please specify the wallet name in the cardanoTestWallet.config object. Example: { walletName: "MyWallet" }'
          );
        const e = await (0, t.mkCardanoWalletExtension)();
        window.cardano = { demos: e };
      })();
    })();
})();
