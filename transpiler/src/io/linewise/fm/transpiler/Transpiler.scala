package io.linewise.fm.transpiler

import scala.meta._

/* =============================================================================
 * STAINLESS -> OX/PRODUCTION TRANSPILER  (scalameta AST-driven)
 *
 * WHICH APPROACH, AND WHY.
 * Rules DETECT structurally on the scalameta AST and EDIT by splicing the
 * original source at node character offsets (`.pos.start`/`.pos.end`). This is
 * the Scalafix "syntactic patch" model: matching is on real syntax (an `Import`
 * node, a `Term.Apply(Term.Name("FMInt"), _)`, a `.ensuring` `Term.Select`),
 * never on regex over raw text — so a token inside a string/comment, a partial
 * identifier, or an unusual line break can no longer trip a rule. Edits splice
 * the ORIGINAL text, so everything we do NOT touch (doc comments, formatting)
 * survives verbatim.
 *
 * On the old Scala-3 wall: scalameta 4.13.4 implements the convenience rewrite
 * helpers (`Tree.transform`/`.traverse`/`.collect`) as Scala-2.13 def-macros
 * that Scala 3 cannot expand. We do NOT use them. Manual `.children` recursion,
 * `.pos`, and case-class pattern matching are plain (non-macro) and work fine —
 * that is the whole engine (`collectEdits`).
 *
 * scalameta is also the VALIDATION GATE on both ends: the input is parsed before
 * any rule runs (a malformed verified source is rejected), and the generated
 * output is re-parsed at the end (a bad edit cannot silently ship). Each rule is
 * its own parse -> collect-edits -> apply pass, so a later rule sees the tree the
 * earlier rules produced (e.g. `.value` erasure runs after FM-ctor erasure).
 *
 * THE RULES (each its own pass, in order):
 *   imports  drop `import stainless.*`, `import io.linewise.verify.effect.{FMInt,FMLong}`,
 *            `import *Proofs.*`; REDIRECT `io.linewise.verify.ox` -> `ox` and
 *            `io.linewise.verify.effect.SafeArith` -> `io.linewise.fm.SafeArith`
 *   R2       rewrite the package ref -> <targetPkg>
 *   R7       strip @law @opaque @inlineOnce @extern @ghost @pure @induct @partialEval
 *   R8       drop whole `def`s whose body contains `.holds`
 *   R5       drop `require(...)` / `assert(...)` / `decreases(...)` statements
 *   R6       drop a trailing `.ensuring(...)` (keep its body)
 *   R3a      FMInt(BigInt(n)) -> n ; FMLong(BigInt(n)) -> nL ; FMInt(e) -> (e).toInt ; ...
 *   R3b      `x.value` -> `x`   (the bounded-value accessor)
 *   R3c      type names FMInt -> Int ; FMLong -> Long
 *   R4       Some[T](x) -> Some(x) ; None[T]() -> None
 * ========================================================================== */
object Transpiler {

  /** Stainless-only annotations stripped by R7. */
  val strippedAnnotations: Set[String] =
    Set("law", "opaque", "inlineOnce", "extern", "ghost", "pure", "induct", "partialEval")

  def header(srcRel: String): String =
    s"""// =============================================================================
       |// GENERATED FROM $srcRel BY transpiler — DO NOT EDIT.
       |//
       |// This is the PRODUCTION business core. It is mechanically derived from the
       |// Stainless-verified source of truth by the `transpiler` Mill module. Every
       |// definition below is the verified definition with the proof-language
       |// substitutions applied (stainless imports dropped, the bounded FM types
       |// FMInt/FMLong ERASED to native Int/Long — their no-overflow bound was
       |// proven in the verifier, so production carries no runtime check —
       |// Some[T](x)/None[T]() -> Some(x)/None, require/ensuring/annotations stripped).
       |// To change the business logic, edit the verified source, re-verify, and
       |// regenerate — never hand-edit this file.
       |// =============================================================================
       |""".stripMargin

  /** Full pipeline. Parse-validate input (Scala3), rewrite, parse-validate output.
    * `targetPkg` is the production package the generated core lands in. */
  def transpile(input: String, srcRel: String, targetPkg: String = "io.linewise.fm.generated"): String = {
    parseScala3(input).fold(
      err => throw new IllegalArgumentException(s"input is not valid Scala 3: $err"),
      _   => ()
    )

    val body = rewrite(input, targetPkg)
    val out  = header(srcRel) + "\n" + body

    parseScala3(out).fold(
      err => throw new IllegalStateException(
        s"generated output is not valid Scala 3 (transpiler bug): $err\n--- output ---\n$out"),
      _   => ()
    )
    out
  }

  /** Parse a string as a Scala 3 Source; Right on success, Left(message) on a
    * parse error. This is the scalameta validation gate. */
  def parseScala3(code: String): Either[String, Source] =
    dialects.Scala3(code).parse[Source] match
      case Parsed.Success(tree) => Right(tree)
      case Parsed.Error(_, msg, _) => Left(msg)

  // ===========================================================================
  // EDIT ENGINE
  // ===========================================================================

  /** A splice into the original source: replace chars [start,end) with `repl`. */
  private final case class Edit(start: Int, end: Int, repl: String)

  /** Apply non-overlapping edits, splicing the original string from the end
    * backwards so earlier offsets stay valid. Throws if two edits overlap (a
    * transpiler bug — rules are designed to produce disjoint spans per pass). */
  private def applyEdits(src: String, edits: Seq[Edit]): String = {
    val sorted = edits.sortBy(-_.start)
    val sb = new StringBuilder(src)
    var lastStart = src.length
    for (e <- sorted) {
      require(e.start <= e.end && e.end <= lastStart,
        s"overlapping/invalid edit $e (next span starts at $lastStart)")
      sb.replace(e.start, e.end, e.repl)
      lastStart = e.start
    }
    sb.toString
  }

  /** OUTERMOST-match traversal: when `pf` matches a node, take its edits and do
    * NOT descend into it (so a node and its descendants never both emit edits).
    * Manual `.children` recursion — NOT the Scala-2.13 macro `.traverse`. */
  private def collectEdits(tree: Tree)(pf: PartialFunction[Tree, List[Edit]]): List[Edit] =
    pf.lift(tree) match
      case Some(edits) => edits
      case None        => tree.children.flatMap(c => collectEdits(c)(pf))

  /** One rule pass: parse, collect edits, apply. */
  private def astRule(s: String)(pf: PartialFunction[Tree, List[Edit]]): String =
    parseScala3(s) match
      case Right(tree) => applyEdits(s, collectEdits(tree)(pf))
      case Left(err)   => throw new IllegalStateException(s"intermediate parse failed (transpiler bug): $err")

  // ---- small helpers --------------------------------------------------------

  private def text(s: String, t: Tree): String = s.substring(t.pos.start, t.pos.end)

  /** Leftmost identifier of a ref: a.b.c -> "a". */
  private def rootName(t: Tree): String = t match
    case Term.Select(q, _) => rootName(q)
    case Term.Name(n)      => n
    case _                 => ""

  /** Dotted path of a ref: a.b.c -> "a.b.c". */
  private def refPath(t: Tree): String = t match
    case Term.Select(q, Term.Name(n)) => val p = refPath(q); if p.isEmpty then n else s"$p.$n"
    case Term.Name(n)                 => n
    case _                            => ""

  /** Expand a node to the whole line(s) it occupies: [start-of-its-first-line,
    * start-of-line-after-its-last-line) — for deleting a statement/import line
    * without leaving a blank line behind. */
  private def lineSpan(src: String, node: Tree): (Int, Int) = {
    var a = node.pos.start
    while a > 0 && src.charAt(a - 1) != '\n' do a -= 1
    var b = node.pos.end
    while b < src.length && src.charAt(b) != '\n' do b += 1
    if b < src.length then b += 1 // include the trailing newline
    (a, b)
  }

  /** Extend an end offset over following spaces/tabs (so deleting `@law ` removes
    * the trailing space too, not just `@law`). */
  private def overSpaces(src: String, end: Int): Int = {
    var e = end
    while e < src.length && (src.charAt(e) == ' ' || src.charAt(e) == '\t') do e += 1
    e
  }

  // ===========================================================================
  // RULES
  // ===========================================================================

  def rewrite(input: String, targetPkg: String = "io.linewise.fm.generated"): String = {
    var s = input
    s = importsRule(s)
    s = packageRule(s, targetPkg)
    s = ghostMembersRule(s)   // before annotationsRule: drop @ghost/@law members whole
    s = annotationsRule(s)
    s = holdsDefsRule(s)
    s = statementsRule(s)
    s = ensuringRule(s)
    s = eraseCtorsRule(s)
    s = eraseValueRule(s)
    s = eraseTypeNamesRule(s)
    s = optionTypeArgsRule(s)
    s
  }

  // --- imports: drop verification-only imports; redirect the two shadows -----
  // Each import here has a single importer. Classified once by its ref path:
  //   io.linewise.verify.ox               -> redirect ref to `ox`
  //   io.linewise.verify.effect.SafeArith -> redirect ref to io.linewise.fm
  //   stainless.*                         -> drop the line
  //   io.linewise.verify.effect.{FMInt,FMLong} -> drop the line
  //   *Proofs.*                           -> drop the line
  private def importsRule(s: String): String = astRule(s) {
    case imp: Import =>
      val er   = imp.importers.head
      val path = refPath(er.ref)
      val root = rootName(er.ref)
      val importees = er.importees.map(_.toString)
      def redirect(to: String) = List(Edit(er.ref.pos.start, er.ref.pos.end, to))
      def dropLine = { val (a, b) = lineSpan(s, imp); List(Edit(a, b, "")) }
      if path == "io.linewise.verify.ox" then
        redirect("ox")
      else if path.startsWith("io.linewise.verify.effect.SafeArith") then
        // `import io.linewise.verify.effect.SafeArith._` (SafeArith is the ref tail)
        redirect(path.replace("io.linewise.verify.effect.SafeArith", "io.linewise.fm.SafeArith"))
      else if path == "io.linewise.verify.effect" && importees.exists(_.contains("SafeArith")) then
        // `import io.linewise.verify.effect.SafeArith` (SafeArith is the importee)
        redirect("io.linewise.fm")
      else if root == "stainless" then dropLine
      else if path == "io.linewise.verify.effect" then dropLine
      else if root.endsWith("Proofs") then dropLine
      else Nil
  }

  // --- R2: rewrite the (outermost) package ref to the target package ---------
  private def packageRule(s: String, targetPkg: String): String = astRule(s) {
    case Pkg(ref, _) => List(Edit(ref.pos.start, ref.pos.end, targetPkg))
  }

  // --- GHOST-ERASURE: drop whole members annotated @ghost or @law -------------
  // These are spec/verification-only and must NOT appear (even de-annotated) in
  // production: a @ghost `rows` model would otherwise materialize a real field, and
  // @law axioms are pure proof obligations. Runs BEFORE annotationsRule (which would
  // strip the @ghost/@law token and hide them). Drops Defn.Def/Decl.Def/Defn.Val
  // (and Decl.Val) carrying the annotation, by line span. The @extern/@pure ops are
  // NOT @ghost/@law, so their real-JDBC bodies survive (annotationsRule strips only
  // their @extern/@pure tokens).
  private val erasedMemberAnnots = Set("ghost", "law")
  private def ghostMembersRule(s: String): String = astRule(s) {
    case d: Defn.Def if hasErasedAnnot(d.mods) => val (a, b) = lineSpan(s, d); List(Edit(a, b, ""))
    case d: Decl.Def if hasErasedAnnot(d.mods) => val (a, b) = lineSpan(s, d); List(Edit(a, b, ""))
    case d: Defn.Val if hasErasedAnnot(d.mods) => val (a, b) = lineSpan(s, d); List(Edit(a, b, ""))
    case d: Decl.Val if hasErasedAnnot(d.mods) => val (a, b) = lineSpan(s, d); List(Edit(a, b, ""))
  }

  private def hasErasedAnnot(mods: List[Mod]): Boolean = mods.exists {
    case a: Mod.Annot => annotName(a).exists(erasedMemberAnnots.contains)
    case _            => false
  }

  // --- R7: strip stainless annotations (the @<name> token + trailing space) --
  private def annotationsRule(s: String): String = astRule(s) {
    case a: Mod.Annot if annotName(a).exists(strippedAnnotations.contains) =>
      List(Edit(a.pos.start, overSpaces(s, a.pos.end), ""))
  }

  private def annotName(a: Mod.Annot): Option[String] = a.init.tpe match
    case Type.Name(n)                 => Some(n)
    case Type.Select(_, Type.Name(n)) => Some(n)
    case _                            => None

  // --- R8: drop whole `def`s whose body contains a `.holds` select -----------
  private def holdsDefsRule(s: String): String = astRule(s) {
    case d: Defn.Def if containsHolds(d) =>
      val (a, b) = lineSpan(s, d); List(Edit(a, b, ""))
  }

  private def containsHolds(t: Tree): Boolean = t match
    case Term.Select(_, Term.Name("holds")) => true
    case _                                   => t.children.exists(containsHolds)

  // --- R5: drop `require(...)` / `assert(...)` / `decreases(...)` statements --
  private val droppedCalls = Set("require", "assert", "decreases")
  private def statementsRule(s: String): String = astRule(s) {
    case t @ Term.Apply(Term.Name(fn), _) if droppedCalls.contains(fn) =>
      val (a, b) = lineSpan(s, t); List(Edit(a, b, ""))
  }

  // --- R6: drop a trailing `.ensuring(...)`, keeping its body ----------------
  private def ensuringRule(s: String): String = astRule(s) {
    case t @ Term.Apply(Term.Select(body, Term.Name("ensuring")), _) =>
      List(Edit(body.pos.end, t.pos.end, ""))
    case t @ Term.ApplyInfix(body, Term.Name("ensuring"), _, _) =>
      List(Edit(body.pos.end, t.pos.end, ""))
  }

  // --- R3a: erase FMInt/FMLong constructors ----------------------------------
  //   FMInt(BigInt(n))  -> n        FMLong(BigInt(n)) -> nL    (numeric literal)
  //   FMInt(e)          -> (e).toInt ; FMLong(e) -> (e).toLong (any other arg)
  private def eraseCtorsRule(s: String): String = astRule(s) {
    case t @ Term.Apply(Term.Name("FMInt"),  List(arg)) =>
      List(Edit(t.pos.start, t.pos.end, ctorRepl(s, arg, "toInt", isLong = false)))
    case t @ Term.Apply(Term.Name("FMLong"), List(arg)) =>
      List(Edit(t.pos.start, t.pos.end, ctorRepl(s, arg, "toLong", isLong = true)))
  }

  private def ctorRepl(s: String, arg: Tree, conv: String, isLong: Boolean): String = arg match
    case Term.Apply(Term.Name("BigInt"), List(inner)) if numericText(s, inner).isDefined =>
      val n = numericText(s, inner).get
      if isLong then s"${n}L" else n
    case _ =>
      s"(${text(s, arg)}).$conv"

  private def numericText(s: String, t: Tree): Option[String] =
    val txt = text(s, t).trim
    if txt.matches("-?\\d+") then Some(txt) else None

  // --- R3b: erase the bounded-value accessor `x.value` -> `x` -----------------
  private def eraseValueRule(s: String): String = astRule(s) {
    case t @ Term.Select(qual, Term.Name("value")) =>
      List(Edit(t.pos.start, t.pos.end, text(s, qual)))
  }

  // --- R3c: erase the FM type names -> native -------------------------------
  private def eraseTypeNamesRule(s: String): String = astRule(s) {
    case t @ Type.Name("FMInt")  => List(Edit(t.pos.start, t.pos.end, "Int"))
    case t @ Type.Name("FMLong") => List(Edit(t.pos.start, t.pos.end, "Long"))
  }

  // --- R4: drop Option/List type applications --------------------------------
  //   None[T]() -> None ; Nil[T]() -> Nil  (whole apply)
  //   Some[T] -> Some ; None[T] -> None ; Nil[T] -> Nil  (just the type args)
  // Nil[T]() appears in real @extern realization bodies (a typed empty accumulator);
  // scala's `Nil` is List[Nothing] and takes no type args, so erase them like Option's.
  private def optionTypeArgsRule(s: String): String = astRule(s) {
    case t @ Term.Apply(Term.ApplyType(Term.Name("None"), _), args) if args.isEmpty =>
      List(Edit(t.pos.start, t.pos.end, "None"))
    case t @ Term.Apply(Term.ApplyType(Term.Name("Nil"), _), args) if args.isEmpty =>
      List(Edit(t.pos.start, t.pos.end, "Nil"))
    case t @ Term.ApplyType(Term.Name("Some"), _) =>
      List(Edit(t.pos.start, t.pos.end, "Some"))
    case t @ Term.ApplyType(Term.Name("None"), _) =>
      List(Edit(t.pos.start, t.pos.end, "None"))
    case t @ Term.ApplyType(Term.Name("Nil"), _) =>
      List(Edit(t.pos.start, t.pos.end, "Nil"))
  }
}
