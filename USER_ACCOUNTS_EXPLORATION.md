# Exploration: Optional User Accounts for Saved Sets

Branch: `explore/user-accounts`

Goal: let a user *optionally* create an account, sign in again later, and
recover the indicator sets they saved. The catalog must stay fully usable
**without** an account (accounts only add persistence).

---

## 1. Short answers

- **Is it possible in R Shiny?** Yes — comfortably. This is a well-trodden
  path; there are mature packages and patterns. Two real sub-problems:
  **(a) authentication** (who is this user) and **(b) persistence** (where do
  their sets live between sessions). (b) is the harder one for us because of
  our hosting.
- **Performance / rendering cost?** Negligible for the interactive parts. Auth
  is a one-time gate at session start; persistence is occasional, debounced
  I/O. **Card rendering, filtering, and selection stay fully in-memory and are
  unaffected.** The only new costs are (i) external network latency on
  login-load and save, (ii) a small dependency/ops footprint, and (iii) a
  slightly slower cold start if the auth package is heavy. None of these touch
  the per-render hot path.

---

## 2. The two hosting realities that shape the design

1. **shinyapps.io has an ephemeral filesystem.** Files written at runtime are
   **not** durable: they vanish on restart/redeploy and are **not shared across
   instances** when the app scales to multiple workers. → We **cannot** store
   accounts or saved sets in a local SQLite file or on disk. Persistence must
   live in an **external service**.
2. **Accounts are optional.** The app is a public catalog. So a full-page login
   wall (the `shinymanager::secure_app()` model) is the wrong shape — it forces
   everyone to authenticate. We want a **"Sign in / Register" affordance in the
   navbar**, a modal login, and guests who keep working with ephemeral
   (session-only) sets exactly as today.

---

## 3. Auth + storage options

| Approach | Auth (register / sign-in / reset) | Where sets live | Handles raw passwords? | Fit for "optional login" | Notes |
|---|---|---|---|---|---|
| **Firebase** (`firebase` R pkg) | Built-in: email+password, verification, reset, Google/social OAuth | Firestore (per-user doc) | No — Google does | Excellent (modal, guest-friendly) | Shiny-native; ties to Google infra we already use |
| **Supabase** (Postgres + GoTrue Auth) | Built-in: email+password, verification, reset, OAuth | Postgres row per user (JSON of sets) | No — Supabase does | Good (custom modal via REST) | SQL/relational, row-level security, generous free tier |
| **polished.tech** (`polished` R pkg) | Hosted: sign-up/sign-in, roles, admin panel | Their service / your DB | No — polished does | Good | Easiest turnkey, but another paid SaaS dependency |
| **shinymanager** | Admin-provisioned users; self-registration is DIY | Encrypted SQLite (❌ ephemeral here) | Yes (you store hashes) | Poor (wrapper = login wall) | Great for gated internal apps, not optional public accounts |
| **Roll your own** (`sodium`/`bcrypt` + external DB) | You build all of it | Any external DB | **Yes — you own this risk** | Full control | Most work + security/PII liability; not recommended |

**Recommendation:** use a **managed auth provider** so we never store or reset
raw passwords (that's the riskiest part of any account system, and a breach
liability). Given we already authenticate to Google with a service account,
**Firebase (Auth + Firestore)** is the most natural, Shiny-native fit.
**Supabase** is an equally strong choice if a relational/SQL store and built-in
row-level security are preferred. Both have free tiers adequate for the beta.

`shinymanager` and custom credential storage are not recommended here: the
former forces a login wall and assumes durable local SQLite; the latter makes
us responsible for password hashing, reset flows, and PII.

---

## 4. Performance / rendering cost — detail

What auth/persistence adds, and where:

- **Session start (one-time):** a login modal + an auth round-trip. For a guest,
  ~zero (no auth). For a returning user, one provider call + **one read** to
  hydrate their sets. Order of a few hundred ms, off the render path.
- **On set change (add/remove/rename/create/delete):** **one debounced write**
  of a small JSON blob (`{set name: [indicator ids]}` — kilobytes). Debounce so
  we persist on meaningful changes, not every reactive tick.
- **Per render (cards, filters, selection):** **no change.** These remain
  in-memory `reactiveVal` operations during the session. We only *hydrate* from
  the store on login and *persist* on change.
- **Memory:** per-user sets are tiny (lists of integer ids). Negligible.
- **Scaling on shinyapps.io:** each session is already its own R worker; auth
  doesn't change the concurrency model. External store handles cross-instance
  consistency (which local disk could not).

Net: the interactive experience users feel today is unchanged. The added
latency is occasional and asynchronous-ish (login load, saves), not in the
filtering/scrolling loop.

---

## 5. How it plugs into *this* codebase

The current set state is a single in-memory reactive in `setManagerServer`:

```r
indicator_sets <- reactiveVal(list("My Indicators" = character()))
active_set_name <- reactiveVal("My Indicators")
```

Everything funnels through this, which makes the integration clean. Plan:

1. **A provider-agnostic store interface** (`R/account_store.R`):
   - `as_register(email, password) -> user_id | error`
   - `as_authenticate(email, password) -> user_id | NULL`
   - `as_load_sets(user_id) -> list`
   - `as_save_sets(user_id, sets) -> invisible()`
   - One backend implementation behind it (Firebase or Supabase). Swapping
     providers later touches only this file.
2. **Auth UI/state in `app.R`**: a navbar "Sign in / Register" button → modal;
   a `reactiveVal(current_user)`; sign-out. Guests never see a wall.
3. **`setManagerServer` hydration + persistence**:
   - On login: `indicator_sets(as_load_sets(user_id))` (fall back to default if
     empty); on sign-out: revert to the guest default.
   - `observe()` on `indicator_sets()` → **debounced** `as_save_sets(...)` when a
     user is signed in (no-op for guests).
   - A "guest" badge / gentle nudge ("Sign in to save your sets") — optional.
4. **Secrets**: provider API keys via `.Renviron` (same pattern as
   `LENS_SHEET_ID` / `GOOGLE_SERVICE_ACCOUNT_KEY`), never committed.

No changes needed to filtering, cards, presets, or the memo drawer.

---

## 6. Security / privacy checklist (beta)

- Use a managed provider → **no raw password storage** on our side.
- HTTPS is provided by shinyapps.io.
- Collect the minimum PII (email only). Add a short privacy note at sign-up.
- Email verification + password reset come free with Firebase/Supabase.
- Saved sets are just indicator-id lists — low sensitivity, but still gated per
  user.
- Confirm with the World Bank / CGAP whether storing user emails needs a
  privacy review or specific data-residency (Firebase/Supabase region choice).

---

## 7. Proposed phased plan

- **Phase 0 (this branch):** this doc + a provider decision. *(you are here)*
- **Phase 1:** `R/account_store.R` interface + chosen backend; secrets wired;
  unit-test register/auth/load/save against the provider.
- **Phase 2:** navbar Sign in / Register modal + `current_user` state; guest
  mode unchanged.
- **Phase 3:** hydrate `setManager` on login; debounced persistence on change;
  sign-out resets to guest.
- **Phase 4:** polish — verification/reset copy, "sign in to save" nudge,
  loading states, error handling, privacy note.

---

## 8. Open decisions (need input)

1. ~~**Provider:** Firebase vs Supabase (vs polished).~~ **DECIDED: Firebase.**
2. ~~**Login methods:**~~ **DECIDED: email + password only.**
3. **Privacy review:** does storing user emails require CGAP/WBG sign-off
   before we ship beyond a test? *(still open)*

---

## 9. DECISION: Firebase (Auth) + Realtime Database — concrete plan

Chosen stack: **Firebase Authentication (email/password)** for accounts, and
**Firebase Realtime Database (RTDB)** for persisting saved sets. Both are driven
by the [`firebase`](https://firebase.john-coene.com) R package (John Coene),
which embeds the Firebase JS SDK and exposes the signed-in user to the Shiny
server. (The package supports RTDB + Storage, *not* Firestore — so we use RTDB.)

### Verified auth API (`firebase` package)

```r
library(firebase)
# server-side:
f <- FirebaseEmailPassword$new()
f$create(email, password)     # register a new account
f$sign_in(email, password)    # sign in
f$sign_out()                  # sign out (inherited)
f$req_sign_in()               # reactive guard: require auth
user <- f$get_signed_in()     # -> list with user$uid, user$email
```

UI deps: `useFirebase()` in the document head + the package's auth containers.

### Config & secrets (`.Renviron`, never committed)

From the Firebase console → Project settings → "Your apps" (Web app) config,
set these (confirm exact names via `?firebase_config` once installed):

```
FIREBASE_API_KEY=...
FIREBASE_PROJECT_ID=cgap-lens-...
FIREBASE_AUTH_DOMAIN=cgap-lens-....firebaseapp.com
FIREBASE_APP_ID=1:...:web:...
FIREBASE_DATABASE_URL=https://cgap-lens-...-default-rtdb.firebaseio.com
```

These are *publishable* client config values (safe to ship to the browser); the
security boundary is the **RTDB security rules**, not secrecy of the API key.

### Firebase console setup (one-time, done by you)

1. Create a Firebase project (can sit in the same Google account as the
   existing service account / `cgap-lens` GCP project).
2. **Authentication → Sign-in method → enable Email/Password.**
3. **Realtime Database → create database.**
4. Set security rules so each user can only read/write their own node:
   ```json
   {
     "rules": {
       "user_sets": {
         "$uid": {
           ".read":  "auth != null && auth.uid === $uid",
           ".write": "auth != null && auth.uid === $uid"
         }
       }
     }
   }
   ```
5. Register a **Web app** and copy its config into `.Renviron` (above).

### Data model (RTDB)

```
/user_sets/$uid = {
  "sets": { "My Indicators": [11, 18, 341], "Credit set": [42] },
  "updated": 1718500000000
}
```

Sets are just lists of indicator ids — tiny, low-sensitivity payloads.

### Integration points (Firebase-specific; see also §5)

- `R/account_store.R` — thin interface: `as_load_sets(uid)` / `as_save_sets(uid,
  sets)` over RTDB; auth handled by `FirebaseEmailPassword` in `app.R`.
- `setManagerServer` — on `get_signed_in()` non-null: hydrate `indicator_sets`
  from `as_load_sets(uid)`; `observe()` (debounced) → `as_save_sets()` while
  signed in; on sign-out, revert to the guest default. Guests = today's
  behaviour (in-memory, no I/O).
- Navbar **"Sign in / Register"** button → modal; no login wall.

### renv / deployment

- Add `firebase` to `renv.lock`. Local `renv::restore()` is currently blocked by
  a broken CommandLineTools C++ toolchain (see session notes), but **shinyapps.io
  builds packages on Posit's servers**, so deployment is unaffected. Local
  testing can use the system R library (binaries) as we've been doing.

### What's needed to proceed to Phase 1

Nothing is wired into `app.R` yet (the live app is untouched). To build and test
the auth + persistence round-trip we need a **Firebase project + its web config
values** in `.Renviron`. Once those exist, the next step is a small **isolated
demo app** (`firebase_demo.R`) proving register → sign-in → save → reload,
*before* touching `setManager` — so the main app is never destabilised by an
untested integration.
