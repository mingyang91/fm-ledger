#!/bin/bash
STAINLESS_VERSION="${STAINLESS_VERSION:-0.9.9.3}"
# Solver portfolio: z3 (fast, proves the vast majority) with cvc5 as a fallback for
# the few VCs z3 returns `unknown` on (recursive Either-returning folds over BigInt,
# e.g. the ledger's postSafe<->post bridge). Both run OUT OF PROCESS: the in-process
# native binding (`nativez3`) segfaults in Z3 4.8.14's model_generator on exactly
# those VCs and takes the JVM down with it, whereas an out-of-process solver crash
# degrades to `unknown` and lets the portfolio's other solver conclude. The VC cache
# is purely structural (solver-independent), so previously-proven VCs still hit cache
# regardless of which solver originally closed them. Override with STAINLESS_SOLVERS.
STAINLESS_SOLVERS="${STAINLESS_SOLVERS:-smt-z3,smt-cvc5}"
STAINLESS_JAR="/opt/stainless/lib/stainless-dotty-standalone-${STAINLESS_VERSION}.jar"
SCALAZ3_JAR="/opt/stainless/lib/scalaz3-unix-64-3.jar"
Z3_JAR="/opt/stainless/lib/com.microsoft.z3.jar"
export PATH="/opt/stainless/z3:/opt/stainless/cvc5:$PATH"
exec java \
  -Djava.library.path="/opt/stainless/lib" \
  -cp "$STAINLESS_JAR:$SCALAZ3_JAR:$Z3_JAR" \
  stainless.Main --solvers="$STAINLESS_SOLVERS" "$@"
