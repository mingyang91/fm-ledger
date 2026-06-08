# Stripe payout integration

The ledger pays recipients through Stripe Connect transfers. The proven accounting decision
(`verify/.../PayoutProofs.recipientPaysFeeSettlement`) is untouched — Stripe lives entirely in the
shell behind `PayoutGateway`, and both edges funnel into that proven settlement.

## The two edges

**Outbound (transactional outbox).** `POST /withdrawals/{id}/submit` records a `PAYOUT_INTENT` and a
`PAYOUT_DISPATCH` row (`status='pending'`) in one DB transaction — it makes no network call. The
`PayoutDispatcher` daemon drains pending rows: it calls `POST /v1/transfers` with an
`Idempotency-Key` of `wd-{id}-intent-{intentId}`, transfers the recipient's **net** amount to the
connected account (`destination=acct_…`), stores the returned `tr_…`, and flips the row to
`dispatched`. A transient failure (5xx/429/network) leaves the row `pending` for the next tick; a
permanent one (4xx) or exhausted attempts flips it to `failed` and raises a `payout_dispatch_failed`
risk event. Because the call is outside any DB transaction and keyed for idempotency, a crash or a
double tick never double-pays.

**Inbound (real webhook).** `POST /stripe/webhook` is public — the `Stripe-Signature` header is the
auth, verified by `StripeSignature.verify` (HMAC-SHA256 over `"{t}.{rawBody}"`, 300 s tolerance,
multiple `v1=` accepted for secret rotation). The raw body is taken as `stringBody` so the bytes
match what Stripe signed. The handler parses the event, maps `transfer.paid`/`payout.paid` →
settled and `transfer.failed`/`payout.failed`/`transfer.reversed` → failed, resolves the withdrawal
from `data.object.metadata.withdrawal_id` (falling back to the stored transfer ref), reads the
observed provider fee, dedups on `(provider, event_id)`, and runs the proven
`settleRecipientPaysFee` / fail path. Unrelated signed events are acknowledged (`handled=false`) so
Stripe stops retrying.

## Observed fee (the one rail-dependent seam)

A Connect transfer carries no Stripe fee itself; the fee surfaces on the connected account's
downstream payout. `feeFromObject` reads `data.object.fee`, then a `balance_transaction.fee`, and
falls back to the quoted fee (→ reconciliation `matched`) when neither is present. When the observed
fee differs from the quote, the reconciliation records `fee_variance` and the ledger expenses the
**observed** fee while recovering the **quoted** fee — the funds invariant still holds.

## Configuration

Set as environment variables on `LedgerServer`:

| Var | Meaning |
|---|---|
| `STRIPE_API_KEY` | `sk_…` secret key. When set, the dispatcher runs; unset, dispatch rows queue. |
| `STRIPE_WEBHOOK_SECRET` | `whsec_…` signing secret (bootstrapped into config; `LEDGER_WEBHOOK_SECRET` is a back-compat alias). |
| `STRIPE_API_BASE_URL` | Default `https://api.stripe.com`. Point at `stripe-mock` in dev/tests. |
| `STRIPE_API_VERSION` | Default `2024-06-20`. |

DB-backed (approved system config; seeded): `payout_currency` (default `usd`),
`payout_points_per_minor_unit` (default `1` — points-to-minor-unit divisor for the transfer amount).

## Testing

Hermetic: `FakeGateway` stands in for outbound transfers, and webhook tests sign a real HMAC over a
fixed `whsec` test secret (no network). The `stripe-mock` Testcontainer validates the actual
`/v1/transfers` request/response shape; it self-skips when the image can't be pulled. `StripeGateway`
talks to it over `java.net.http` — there is no production HTTP-client dependency and no Stripe SDK.
