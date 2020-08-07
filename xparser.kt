package xparser

import java.io.File

class PResult(val s: Boolean, val p: Int, val r: List<Any>)
class PToken(val type: String, val value: String)
typealias PFun = (Int) -> PResult
typealias PPFun = (List<Any>) -> List<Any>

fun tokenizeCharSource(cs: String) = cs.map { PToken("c", "$it") }

abstract class Parser(val tokens: List<PToken>) {

    fun sequential(vararg fs: PFun): PFun = point@ {
        var p = it
        val store = mutableListOf<Any>()
        for (f in fs) {
            val result = f(p)
            if (!result.s) return@point PResult(false, it, listOf())
            store.add(result.r)
            p = result.p
        }
        PResult(true, p, store.toList())
    }

    fun parallel(vararg fs: PFun): PFun = point@ {
        for (f in fs) {
            val result = f(it)
            if (result.s) return@point result
        }
        PResult(false, it, listOf())
    }

    fun oneOrMore(f: PFun): PFun = point@ {
        val store = mutableListOf<Any>()
        var result = f(it)
        if (!result.s) return@point PResult(false, it, listOf())
        while (result.s) {
            store.add(result.r)
            result = f(result.p)
        }
        PResult(true, result.p, store.toList())
    }

    private val nothing: PFun = { PResult(true, it, listOf()) }
    fun optional(f: PFun) = parallel(f, nothing)
    fun more(f: PFun): PFun = optional(oneOrMore(f))
    //fun oneOrMore(f: xparser.PFun) = sequential(f, more(f))

    fun flattenList(content: List<Any>): List<Any> = content.map {
        @Suppress("UNCHECKED_CAST")
        if (it !is List<*>) listOf(it) else flattenList(it as List<Any>)
    }.flatten()

    fun attachType(type: String, f: PFun): PFun = {
        val result = f(it)
        if (!result.s) result
        else PResult(true, result.p, listOf(type, result.r))
    }

    fun myFlattenString(content: Any): String {
        if (content !is List<*>) return "$content"
        @Suppress("UNCHECKED_CAST")
        val a = content as List<Any>
        return a.joinToString("") { myFlattenString(it) }
    }

    val getToken: PFun = {
        if (it == tokens.size) PResult(false, it, listOf())
        else PResult(true, it + 1, listOf(tokens[it]))
    }

    fun checkToken(good: (PToken) -> Boolean): PFun = {
        val result = getToken(it)
        if (!result.s) result
        else if (good(result.r[0] as PToken)) result
        else PResult(false, it, listOf())
    }

    fun post(f: PFun, pp: (PResult, Int) -> PResult): PFun = {
        val result = f(it)
        if (result.s) pp(result, it) else result
    }

    fun postr(f: PFun, pp: (List<Any>) -> List<Any>): PFun = post(f) { x: PResult, p: Int ->
        PResult(x.s, x.p, pp(x.r))
    }

    fun negate(f: PFun, g: PFun): PFun = {
        if (f(it).s) PResult(false, it, listOf()) else g(it)
    }

    fun neg(f: PFun) = negate(f, getToken)

    fun ttype(t: String): PFun = checkToken { it.type.equals(t) }
    fun makeToken(t: String): PPFun = {
        val x = myFlattenString(flattenList(it).map { i: Any -> (i as PToken).value })
        listOf(PToken(t, x))
    }
    val make_token_identifier = makeToken("identifier")
    val make_token_keyword = makeToken("keyword")
    val make_token_space = makeToken("space")
    val make_token_string = makeToken("string")
    val make_token_symbol = makeToken("symbol")
    val make_token_number = makeToken("number")
    val make_token_line_comment = makeToken("line_comment")
    val make_token_block_comment = makeToken("block_comment")
    val is_alpha = checkToken { it.value[0].isLetter() }
    val is_digit = checkToken { it.value[0].isDigit() }
    val is_space = checkToken { it.value[0].isWhitespace() }
    fun ch(ch: Char) = checkToken { it.value[0] == ch }
    val any = getToken
    val symbol = any
    val quote = ch('\'')
    val dquote = ch('"')
    val line_comment_end = parallel(sequential(ch('\r'),ch('\n')),sequential(ch('\n'),ch('\r')),ch('\r'),ch('\n'))
    val line_comment_char = neg(parallel(ch('\r'), ch('\n')))
    val block_comment_char = neg(sequential(ch('*'),ch('/')))

    abstract fun parse(): PResult
}

class LexParser(tokens: List<PToken>): Parser(tokens) {
    val removeTypes = listOf("space", "line_comment", "block_comment")
    val make_token_ysymbol = makeToken("ysymbol")
    val make_token_ykeyword = makeToken("ykeyword")

    val lex: PFun = more{one_token(it)}
    val one_token: PFun = parallel(postr({identifier(it)}){make_token_identifier(it)}, postr({space(it)}){make_token_space(it)}, postr({ysymbol(it)}){make_token_ysymbol(it)}, postr({ykeyword(it)}){make_token_ykeyword(it)}, postr({line_comment(it)}){make_token_line_comment(it)}, postr({block_comment(it)}){make_token_block_comment(it)}, postr({symbol(it)}){make_token_symbol(it)})
    val identifier: PFun = sequential({identifier_first_char(it)}, more{identifier_next_char(it)})
    val identifier_first_char: PFun = parallel({is_alpha(it)}, ch('_'))
    val identifier_next_char: PFun = parallel({identifier_first_char(it)}, {is_digit(it)})
    val space: PFun = oneOrMore{is_space(it)}
    val ysymbol: PFun = sequential({quote(it)}, {any(it)}, {quote(it)})
    val ykeyword: PFun = sequential({dquote(it)}, {identifier(it)}, {dquote(it)})
    val line_comment: PFun = sequential(ch('/'), ch('/'), more{line_comment_char(it)}, {line_comment_end(it)})
    val block_comment: PFun = sequential(ch('/'), ch('*'), more{block_comment_char(it)}, ch('*'), ch('/'))

    override fun parse(): PResult {
        val result = lex(0)
        val r = flattenList(result.r).filter { (it as PToken).type !in removeTypes }
        return PResult(result.s, result.p, r)
    }

}

class YaccParser(tokens: List<PToken>): Parser(tokens) {
    val definedTerms = mutableSetOf<String>()

    val p1: PPFun = { listOf("{${(it[0] as PToken).value}(it)}") }
    val p2: PPFun = { listOf("${(it[0] as PToken).value}") }
    val ps1: PPFun = {
        val t = it[0] as PToken
        val ch = t.value[1] as Char
        val ret = "ch('$ch')"
        listOf(ret)
    }
    val ps2: PPFun = {
        val t = it[0] as PToken
        val kx = t.value
        val k = kx.substring(1..kx.length-1)
        val ret = "kword(\"$k\")"
        listOf(ret)
    }

    val pzero: PPFun = { listOf("more${flattenList(it)[0]}") }
    val pmore: PPFun = { listOf("oneOrMore${flattenList(it)[0]}") }
    val poptional: PPFun = { listOf("optional${flattenList(it)[0]}") }
    val pskip: PPFun = { listOf(it[1]) }
    val pcollect: PPFun = { flattenList(it) }

    fun pseq(r: List<Any>): List<Any> {
        val x = flattenList(r)
        val ret = if (x.size == 1) x[0] as String else "sequential(${x.joinToString(", ")})"
        return listOf(ret)
    }
    fun ppost(r: List<Any>): List<Any> {
        val x = flattenList(r)
        val ret = if (x.size == 1) x[0] as String else "postr(${x[0]})${x[1]}"
        return listOf(ret)
    }
    fun pcombine(r: List<Any>): List<Any> {
        val x = flattenList(r)
        val ret = if (x.size == 1) x[0] as String else "parallel(${x.joinToString(", ") { it as String}})"
        return listOf(ret)
    }
    fun pdef(r: List<Any>): List<Any> {
        val x = flattenList(r)
        definedTerms.add(x[0] as String)
        val ret = "    val ${x[0]}: PFun = ${x[2]}"
        return listOf(ret)
    }
    val y_symbol = postr(ttype("ysymbol"), ps1)
    val y_keyword = postr(ttype("ykeyword"), ps2)
    val y_identifier = postr(ttype("identifier"), p1)
    val y_entry_name = postr(ttype("identifier"), p2)

    val y_grammar: PFun = oneOrMore{y_entry(it)}
    val y_entry: PFun = postr(sequential({y_entry_name(it)}, ch('='), {y_entry_def(it)}, ch(';'))){pdef(it)}
    val y_entry_def: PFun = postr(sequential({y_sequence(it)}, {y_or_sequences(it)})){pcombine(it)}
    val y_or_sequences: PFun = postr(more{y_or_sequence(it)}){pcollect(it)}
    val y_or_sequence: PFun = postr(sequential(ch('|'), {y_sequence(it)})){pskip(it)}
    val y_sequence: PFun = postr(sequential({y_seq_objects(it)}, optional{y_post_processing(it)})){ppost(it)}
    val y_post_processing: PFun = postr(sequential({p_post_op(it)}, {y_processing(it)})){pskip(it)}
    val p_post_op: PFun = sequential(ch('-'), ch('>'))
    val y_seq_objects: PFun = postr(oneOrMore{y_seq_object(it)}){pseq(it)}
    val y_seq_object: PFun = parallel(postr(sequential({y_term(it)}, ch('*'))){pzero(it)}, postr(sequential({y_term(it)}, ch('+'))){pmore(it)}, postr(sequential({y_term(it)}, ch('?'))){poptional(it)}, {y_term(it)}, {y_symbol(it)}, postr(sequential({y_keyword(it)}, ch('?'))){poptional(it)}, {y_keyword(it)})
    val y_term: PFun = {y_identifier(it)}
    val y_processing: PFun = {y_identifier(it)}

    override fun parse(): PResult {
        val result = y_grammar(0)

        val usedTerms = tokens.filter { it.type.equals("identifier") }.map { it.value }.toSet()
        val undefinedTerms = usedTerms.minus(definedTerms).toList().sorted()
        println("undefined terms:")
        undefinedTerms.forEach { println(it) }
        println()

        return PResult(result.s, result.p, flattenList(result.r))
    }

}

fun main() {
    val charSource = File("data/pyacc.txt").readText()
    val charTokens = tokenizeCharSource(charSource)
    val lexParser = LexParser(charTokens)
    val lexTokens = lexParser.parse().r.map { it as PToken }
    //lexTokens.forEach { println(it) }
    val yaccParser = YaccParser(lexTokens)
    val result = yaccParser.parse()
    println("${result.s} ${result.p}")
    result.r.map { it as String }.forEach { println(it) }
}
