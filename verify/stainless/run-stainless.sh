#!/bin/bash
STAINLESS_VERSION="${STAINLESS_VERSION:-0.9.9.3}"
STAINLESS_SOLVERS="${STAINLESS_SOLVERS:-nativez3}"
STAINLESS_JAR="/opt/stainless/lib/stainless-dotty-standalone-${STAINLESS_VERSION}.jar"
SCALAZ3_JAR="/opt/stainless/lib/scalaz3-unix-64-3.jar"
Z3_JAR="/opt/stainless/lib/com.microsoft.z3.jar"
export PATH="/opt/stainless/z3:/opt/stainless/cvc5:$PATH"
exec java \
  -Djava.library.path="/opt/stainless/lib" \
  -cp "$STAINLESS_JAR:$SCALAZ3_JAR:$Z3_JAR" \
  stainless.Main --solvers="$STAINLESS_SOLVERS" "$@"
