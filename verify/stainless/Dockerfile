# =============================================================================
# Stainless Formal Verification Container (ARM64 + native Z3 / ScalaZ3)
# =============================================================================
#
# Multi-stage build:
#   1. build-z3:       Compile Z3 4.8.14 from source with Java bindings (ARM64)
#   2. build-scalaz3:  Package ScalaZ3 with arm64 native libraries
#   3. runtime:        Stainless standalone 0.9.9.3 + native Z3 / ScalaZ3
#
# Usage:
#   docker build -t stainless-verify verify/stainless/
#   docker run --rm -v $(pwd)/verify:/work stainless-verify \
#     /work/stainless-lib/IO.scala /work/stainless-lib/EitherT.scala \
#     /work/src/main/scala/dev/mingyang91/verify/mvp/GivenWorld.scala
# =============================================================================

ARG JAVA_VERSION=17

# ---------------------------------------------------------------------------
# Stage 1: Build Z3 from source with Java bindings
# ---------------------------------------------------------------------------
# Pin to jammy (Ubuntu 22.04, GCC 11) because z3 4.8.14 ships a typo
# (`m_low_bound` instead of `m_lower_bound`) that GCC 13/14 — the default on
# the unsuffixed `:17-jdk` tag, which now points at Ubuntu 24.04 — rejects as
# a hard error. Older GCC treats it as a warning. Upstream z3 fixed this on
# master, but ScalaZ3 is pinned to z3 4.8.14, so we work around at the
# toolchain level.
FROM eclipse-temurin:${JAVA_VERSION}-jdk-jammy AS build-z3

RUN apt-get update && apt-get install -y --no-install-recommends \
    git cmake python3 make g++ unzip curl ca-certificates \
    && rm -rf /var/lib/apt/lists/*

ARG Z3_VERSION=4.8.14
ARG Z3_TAG=z3-${Z3_VERSION}

WORKDIR /build

RUN git clone --depth 1 --branch "${Z3_TAG}" https://github.com/Z3Prover/z3.git

WORKDIR /build/z3
RUN python3 scripts/mk_make.py --java \
    && cd build \
    && make -j$(nproc) \
    && ls -la /build/z3/build/libz3.so \
              /build/z3/build/libz3java.so \
              /build/z3/build/com.microsoft.z3.jar

# ---------------------------------------------------------------------------
# Stage 2: Build ScalaZ3 bundle with arm64 native libraries
# ---------------------------------------------------------------------------
# Same jammy pin as build-z3: ScalaZ3's sbt build re-runs parts of the z3 C++
# compilation when packaging the JNI bindings, so the GCC version constraint
# applies here too.
FROM eclipse-temurin:${JAVA_VERSION}-jdk-jammy AS build-scalaz3

RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 make g++ curl ca-certificates unzip \
    && rm -rf /var/lib/apt/lists/* \
    && ln -sf /usr/bin/python3 /usr/local/bin/python \
    && curl -fsSL https://raw.githubusercontent.com/dwijnand/sbt-extras/master/sbt \
       -o /usr/local/bin/sbt \
    && chmod +x /usr/local/bin/sbt

ARG SCALAZ3_REPO=https://github.com/epfl-lara/ScalaZ3.git
ARG SCALAZ3_REF=master

WORKDIR /build

RUN git clone --depth 1 --branch "${SCALAZ3_REF}" "${SCALAZ3_REPO}" ScalaZ3
# Preseed the tagged Z3 checkout so ScalaZ3's sbt build does not re-fetch it via JGit.
COPY --from=build-z3 /build/z3 /build/ScalaZ3/z3/z3-4.8.14
RUN git -C /build/ScalaZ3/z3/z3-4.8.14 apply /build/ScalaZ3/z3_mk_abs.patch
RUN python3 - <<'PY'
import hashlib
import os
from pathlib import Path

base = Path("/build/ScalaZ3/z3/z3-4.8.14")
src = base / "src"
files = []

for root, dirs, filenames in os.walk(src):
    dirs[:] = [d for d in dirs if not d.startswith(".")]
    for name in filenames:
        if name.startswith(".") or name.endswith(".pyc") or name.endswith(".a"):
            continue
        files.append(Path(root) / name)

digest = hashlib.md5()
for path in sorted(files, key=lambda p: str(p)):
    digest.update(str(path).encode())
    digest.update(path.read_bytes())

value = "".join(format(byte, "x") for byte in digest.digest())
(base / ".build-hash").write_text(value)
print(value)
PY

WORKDIR /build/ScalaZ3
RUN sbt -Dsbt.log.noformat=true +package \
    && cp target/scala-3.2.0/scalaz3_3-4.8.14.jar /build/scalaz3-unix-64-3.jar \
    && ls -la /build/scalaz3-unix-64-3.jar \
              /build/ScalaZ3/lib-bin/libscalaz3.so

# ---------------------------------------------------------------------------
# Stage 3: Runtime — Stainless + native Z3
# ---------------------------------------------------------------------------
FROM eclipse-temurin:${JAVA_VERSION}-jre AS runtime

ARG STAINLESS_VERSION=0.9.9.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl unzip ca-certificates libgomp1 \
    && rm -rf /var/lib/apt/lists/* \
    && curl --retry 3 --retry-all-errors -fsSL -o /tmp/stainless.zip \
       "https://github.com/epfl-lara/stainless/releases/download/v${STAINLESS_VERSION}/stainless-dotty-standalone-${STAINLESS_VERSION}-linux.zip" \
    && unzip -o /tmp/stainless.zip -d /opt/stainless \
    && rm /tmp/stainless.zip \
    && chmod +x /opt/stainless/stainless

WORKDIR /opt/stainless

# Replace release x64 ScalaZ3 bundle and copy matching ARM64 native libraries
COPY --from=build-z3 /build/z3/build/z3 /opt/stainless/z3/z3
COPY --from=build-z3 /build/z3/build/libz3.so /opt/stainless/lib/
COPY --from=build-z3 /build/z3/build/libz3java.so /opt/stainless/lib/
COPY --from=build-z3 /build/z3/build/com.microsoft.z3.jar /opt/stainless/lib/
COPY --from=build-scalaz3 /build/scalaz3-unix-64-3.jar /opt/stainless/lib/scalaz3-unix-64-3.jar
COPY --from=build-scalaz3 /build/ScalaZ3/lib-bin/libscalaz3.so /opt/stainless/lib/

# Configure, set permissions, and pass version to launcher
ENV STAINLESS_VERSION=${STAINLESS_VERSION}
RUN chmod +x /opt/stainless/z3/z3 \
    && cat > /opt/stainless/stainless.conf <<'EOF'
vc-cache     = true
timeout      = 120
check-models = false
print-ids    = false
print-types  = false
batched      = false
solvers      = "nativez3"
EOF
COPY run-stainless.sh /opt/stainless/run-stainless.sh
RUN chmod +x /opt/stainless/run-stainless.sh

ENV LD_LIBRARY_PATH="/opt/stainless/lib"

ENTRYPOINT ["/opt/stainless/run-stainless.sh"]
