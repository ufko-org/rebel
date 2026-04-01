/*  primes.h - table of primitives for Rebel

    Copyright (C) 2015 Lutz Mueller
    Copyright (C) 2025 Ufko (ufko.org)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ---------------------------------------------------------------------

    Author's musings:
    =================


    This table defines the active Rebel language interface.

    Rebel encourages users to adapt the language to their needs.
    Creating aliases for built-in functions is normal daily practice.
    It is an idiom and has no speed penalty.

    Aliases also provide long-term stability. If a function name
    changes in the future, only the alias definition needs to be
    updated and existing code keeps working. The best place for
    user created aliases is ~/.init.rbl file.

    (alias 'bt sta.bayes-train) for current context
    (alias (shared 'bt) sta.bayes-train) for all contexts

    Also there is no module system at the API level; modularity
    is a userland concept implemented explicitly via contexts.

    ---

    In an extreme case, users can define their own naming
    directly in primes.h by providing alternative (:alt)
    names mapped to the same C primitives.

    In this setup, Rebel provides the runtime engine,
    while users provide the vocabulary at compile time.

    ---------------------------------------------------------------------

    Function naming notes (this is harder than it looks):

    1) A function that may be used standalone but is mostly used as an
       argument to another function should be named after what it returns.

    2) A function that is typically used standalone or mapped/applied by
       another function should be named after what it does.

    Function name patterns, if possible:

       noun          - string/str            (conversion operation)
       action        - rotate/rot            (generic operation)
       nounaction    - stringrotate/strrot   (specific operation)
       actionnoun    - rotatestring/rotstr   (worst case)
       value         - crc32                 (computed value / algo name)

    Special form names:

       actionfamily  - dolist, dostring, do*

    Core function names should be as short as possible while still allowing
    a reasonable guess of what they do or return. Fancy characters other
    than '?' are reserved for wild userland.

    Allowed characters in core names: [a-z0-9?]

    Any brand new function, if added, is marked with a :rebel comment
    next to it, indicating a new C implementation with new semantics.

    Some functions may have alternative names marked :alt that provide
    clearer meaning in a given context or improve code reading and writing
    flow. Alternatives can replace original functions in future.

    constant -> const, alias
    =        -> eq, is
    true?    -> ok
    null?    -> no

    primes.h is the authoritative source.
*/

#ifndef PRIMES_H
#define PRIMES_H

#ifdef DOT_NAMES
  #define P(name) name "."
#else
  #define P(name) name
#endif

PRIMITIVE primitive[] =
{
    /* CORE ------------------------------------------------------------- */

    /* core - data - symbol workers */

    {P("const"),            p_constant,         0x400},
    {P("alias"),            p_constant,         0x400}, /* :alt */
    {P("shared"),           p_shared,           0},     /* :replaces global */
    {P("let"),              p_let,              0x402},
    {P("letex"),            p_letExpand,        0x403},
    {P("letn"),             p_letn,             0x002},
    {P("local"),            p_local,            2},
    {P("func"),             p_func,             0x402}, /* :rebel :replaces define */
    {P("def"),              p_func,             0x402}, /* :rebel :alt func */
    {P("set"),              p_setdef,           0x400}, /* :rebel - set place to value */
    {P("setn"),             p_setn,             0x400}, /* :rebel - set 'name to value */
    {P("mut"),              p_mut,              0},     /* [] :rebel, opt-in mutate tool */
    {P("mutl"),             p_mutLocal,         0},     /* [] :rebel, opt-in stack mutate tool */
    {P("mac"),              p_defineMacro,      0x402}, /* runtime macro, :replaces define-macro */
    {P("macex"),            p_macro,            2},     /* expand macro, :replaces macro. */

    /* core - data - iterators */

    {P("loop"),             p_loop,             2}, /* [l,s,a,c] :rebel */
    {P("forl"),             p_dolist,           2}, /* [l,a] for list/array elements */
    {P("fors"),             p_dostring,         2}, /* [s] for string code points */
    {P("forf"),             p_doargs,           2}, /* [l] for function args */
    {P("forc"),             p_dotree,           2}, /* [c] for context symbols */

    /* core - data - sequencers */

    {P("array"),            p_array,            0},
    {P("list"),             p_list,             0},
    {P("range"),            p_sequence,         0},
    {P("series"),           p_series,           0},

    /* core - data - convertors/extractors */

    {P("arraylist"),        p_arrayList,        0},
    {P("bigint"),           p_bigInt,           0},
    {P("bits"),             p_bits,             0},
    {P("char"),             p_char,             0},
    {P("float"),            p_float,            0},
    {P("int"),              p_integer,          0},
    {P("string"),           p_string,           0},

    /* core - data - transformers */

    {P("b64dec"),           p_base64Dec,        0}, /* :replaces base64-dec */
    {P("b64enc"),           p_base64Enc,        0}, /* :replaces base64-enc */
    {P("crc32"),            p_crc32,            0},
    {P("crypt"),          p_encrypt,            0}, /* :replaces encrypt - bidir OTP cryptor */
    {P("uuid"),             p_uuid,             0},

    /* core - data - workers */

    {P("all"),              p_forAll,           0},
    {P("any"),              p_any,              0},
    {P("append"),           p_append,           0},     /* [l,s,a] */
    {P("apply"),            p_apply,            0},     /* [l] */
    {P("assoc"),            p_assoc,            0},     /* [l] */
    {P("bind"),             p_bind,             0x400}, /* [l] */
    {P("intersect"),        p_intersect,        0},     /* [l] */
    {P("check"),            p_exists,           0},     /* [l] :replaces exists */
    {P("chop"),             p_chop,             0},     /* [l,s] */
    {P("chunk"),            p_explode,          0},     /* [l,s] :replaces explode*/
    {P("collect"),          p_collect,          0},     /* [] */
    {P("cons"),             p_cons,             0},     /* [l] */
    {P("curry"),            p_curry,            0},     /* [] */
    {P("cut"),              p_member,           0},     /* [l,s,a] */
    {P("diff"),             p_difference,       0},     /* [l] :replaces difference */
    {P("drop"),             p_clean,            0},     /* [l] :replaces clean */
    {P("dup"),              p_dup,              0},     /* [any] */
    {P("ends"),             p_endsWith,         0},     /* [l,s] :replaces ends-with */
    {P("expand"),           p_expand,           0},     /* [l] */
    {P("extend"),           p_extend,           0x400}, /* [l,s] */
    {P("find"),             p_findAll,          0},     /* [l,s] :replaces findall */
    {P("first"),            p_first,            0},     /* [l,s,a] */
    {P("flat"),             p_flat,             0},     /* [l] */
    {P("format"),           p_format,           0},     /* [s] */
    {P("freq"),             p_count,            0},     /* [l] */
    {P("join"),             p_join,             0},     /* [l] */
    {P("keep"),             p_filter,           0},     /* [l] :replaces filter */
    {P("last"),             p_last,             0},     /* [l,s,a] */
    {P("lcase"),            p_lower,            0},     /* [s] :replaces lower-case */
    {P("len"),              p_length,           0},     /* [any] */
    {P("lookup"),           p_lookup,           0},     /* [l] */
    {P("map"),              p_map,              0},     /* [l] */
    {P("match"),            p_match,            0},     /* [l] */
    {P("nth"),              p_nth,              0},     /* [] */
    {P("parse"),            p_parse,            0},     /* [] */
    {P("pop"),              p_pop,              0x400}, /* [] */
    {P("popassoc"),         p_popAssoc,         0x400}, /* [] */
    {P("pos"),              p_find,             0},     /* [l,s] :replaces find - pos by value */
    {P("posp"),             p_index,            0},     /* [l] :replaces index - positions by predicate */
    {P("push"),             p_push,             0x400}, /* [] */
    {P("ref"),              p_ref,              0},     /* [] */
    {P("refall"),           p_refAll,           0},     /* [] */
    {P("refset"),           p_setRef,           0x400}, /* [] :replaces set-ref */
    {P("refsetall"),        p_setRefAll,        0x400}, /* [] :replaces set-ref-all */
    {P("replace"),          p_replace,          0x400}, /* [] */
    {P("rest"),             p_rest,             0},     /* [] */
    {P("reverse"),          p_reverse,          0x400}, /* [] */
    {P("rotate"),           p_rotate,           0x400}, /* [] */
    {P("rx"),               p_regex,            0},     /* [] :replaces regex */
    {P("rxcomp"),           p_regexComp,        0},     /* [] :replaces regex-comp */
    {P("select"),           p_select,           0},     /* [] */
    {P("slice"),            p_slice,            0},     /* [] */
    {P("sort"),             p_sort,             0x400}, /* [] */
    {P("starts"),           p_startsWith,       0},     /* [l,s] :replaces starts-with */
    {P("swap"),             p_swap,             0},     /* [] */
    {P("tcase"),            p_title,            0},     /* [] :replaces title-case */
    {P("trim"),             p_trim,             0},     /* [] */
    {P("ucase"),            p_upper,            0},     /* [] :replaces upper-case */
    {P("unify"),            p_unify,            0},     /* [] */
    {P("union"),            p_union,            0},     /* [] */
    {P("unique"),           p_unique,           0},     /* [] */

    #ifdef SUPPORT_UTF8
    {P("len8"),             p_utf8len,          0},     /* [s] */
    #endif

    /* core - math - integers */

    {P("+"),                p_add,              0},
    {P("-"),                p_subtract,         0},
    {P("*"),                p_multiply,         0},
    {P("/"),                p_divide,           0},
    {P("%"),                p_modulo,           0},
    {P("++"),               p_incrementI,       0x400},
    {P("--"),               p_decrementI,       0x400},

    /* core - math - floats */

    {P("add"),              p_addFloat,         0},
    {P("sub"),              p_subFloat,         0},
    {P("mul"),              p_mulFloat,         0},
    {P("div"),              p_divFloat,         0},
    {P("mod"),              p_modFloat,         0},
    {P("inc"),              p_incrementF,       0x400},
    {P("dec"),              p_decrementF,       0x400},

    /* core - math - floats extended */

    {P("abs"),              p_abs,              0},
    {P("acos"),             p_acos,             0},
    {P("acosh"),            p_acosh,            0},
    {P("asin"),             p_asin,             0},
    {P("asinh"),            p_asinh,            0},
    {P("atan"),             p_atan,             0},
    {P("atan2"),            p_atan2,            0},
    {P("atanh"),            p_atanh,            0},
    {P("ceil"),             p_ceil,             0},
    {P("cos"),              p_cos,              0},
    {P("cosh"),             p_cosh,             0},
    {P("erf"),              p_erf,              0},
    {P("exp"),              p_exp,              0},
    {P("factor"),           p_factor,           0},
    {P("floor"),            p_floor,            0},
    {P("gcd"),              p_gcd,              0},
    {P("log"),              p_log,              0},
    {P("max"),              p_maxFloat,         0},
    {P("min"),              p_minFloat,         0},
    {P("pow"),              p_powFloat,         0},
    {P("round"),            p_round,            0},
    {P("sgn"),              p_sgn,              0},
    {P("sin"),              p_sin,              0},
    {P("sinh"),             p_sinh,             0},
    {P("sqrt"),             p_sqrt,             0},
    {P("ssq"),              p_ssq,              0},
    {P("tan"),              p_tan,              0},
    {P("tanh"),             p_tanh,             0},

    /* core - flow */

    {P("case"),             p_case,             2},
    {P("catch"),            p_catch,            0},
    {P("cond"),             p_condition,        2}, 
    {P("do"),               p_evalBlock,        1}, /* :replaces begin */
    {P("dountil"),          p_doUntil,          2},
    {P("dowhile"),          p_doWhile,          2},
    {P("for"),              p_for,              2},
    {P("if"),               p_if,               2},
    {P("ifnot"),            p_ifNot,            2}, /* :rebel full, multi-branch if counterpart */
    {P("repeat"),           p_dotimes,          2}, /* :replaces dotimes */
    {P("throw"),            p_throw,            0},
    {P("unless"),           p_unless,           2},
    {P("until"),            p_until,            2},
    {P("when"),             p_when,             2},
    {P("while"),            p_while,            2},

    /* core - logical */

    {P("and"),              p_and,              0},
    {P("or"),               p_or,               0},
    {P("not"),              p_not,              0},

    /* core - comparison ops */

    {P("<"),                p_less,             0},
    {P("lt"),               p_less,             0}, /* :alt */
    {P(">"),                p_greater,          0},
    {P("gt"),               p_greater,          0}, /* :alt */
    {P("<="),               p_lessEqual,        0},
    {P("le"),               p_lessEqual,        0}, /* :alt */
    {P(">="),               p_greaterEqual,     0},
    {P("ge"),               p_greaterEqual,     0}, /* :alt */
    {P("="),                p_equal,            0},
    {P("eq"),               p_equal,            0}, /* :alt */
    {P("!="),               p_notEqual,         0},
    {P("ne"),               p_notEqual,         0}, /* :alt */

    /* core - bit ops */

    {P("<<"),               p_shiftLeft,        0},
    {P(">>"),               p_shiftRight,       0},
    {P("&"),                p_bitAnd,           0},
    {P("|"),                p_bitOr,            0},
    {P("^"),                p_bitXor,           0},
    {P("~"),                p_bitNot,           0},

    /* core - random */

    {P("pick"),             p_amb,              0}, /* :replaces amb */
    {P("normal"),           p_normal,           0},
    {P("rand"),             p_rand,             0},
    {P("random"),           p_random,           0},
    {P("randomize"),        p_randomize,        0},
    {P("seed"),             p_seed,             0},

    /* core - io - via std */

    {P("print"),            p_print,            0},
    {P("println"),          p_println,          0},
    {P("puts"),             p_println,          0}, /* :alt */
    {P("readkey"),          p_readKey,          0},

    /* core - io - via device number */

    {P("device"),           p_device,           0},
    {P("open"),             p_open,             0},
    {P("close"),            p_close,            0},
    {P("seek"),             p_seek,             0},
    #ifdef SUPPORT_UTF8
    {P("readc8"),           p_readUTF8,         0},
    #endif
    {P("read"),             p_readBuffer,       0x400},
    {P("write"),            p_writeBuffer,      0},
    {P("readc"),            p_readChar,         0},
    {P("writec"),           p_writeChar,        0},
    {P("readln"),           p_readLine,         0},
    {P("writeln"),          p_writeLine,        0},
    {P("cline"),            p_currentLine,      0},

    /* core - io - via path */

    {P("fpath"),            p_realpath,         0},
    {P("finfo"),            p_fileInfo,         0},
    {P("fappend"),          p_appendFile,       0},
    {P("fread"),            p_readFile,         0},
    {P("fwrite"),           p_writeFile,        0},
    {P("fcopy"),            p_copyFile,         0},
    {P("fmove"),            p_renameFile,       0},
    {P("fdel"),             p_deleteFile,       0},
    {P("fsearch"),          p_search,           0},

    /* core - io - directories */

    {P("dir"),              p_directory,        0},
    {P("dirpath"),          p_realpath,         0},
    {P("dirmk"),            p_makeDir,          0},
    {P("dirrm"),            p_removeDir,        0},
    {P("dircd"),            p_changeDir,        0},

    /* core - OS/CILK processes */

    {P("!"),                p_system,           0},
    {P("kill"),             p_destroyProcess,   0}, /* :replaces destroy */
    {P("exec"),             p_exec,             0},
    {P("process"),          p_process,          0},
    {P("pipe"),             p_pipe,             0},
    {P("fork"),             p_fork,             0},
    {P("waitpid"),          p_waitpid,          0},
    {P("spawn"),            p_spawn,            0},
    {P("sync"),             p_sync,             0},
    {P("abort"),            p_abort,            0},
    {P("send"),             p_send,             0},
    {P("recv"),             p_receive,          0},
    {P("signal"),           p_signal,           0},
    {P("shmem"),            p_share,            0}, /* :replaces share */
    {P("semaphore"),        p_semaphore,        0},
    {P("peek"),             p_peek,             0},

    /* core - internals */

    {P("$"),                p_systemSymbol,     0},
    {P("adhoc"),            p_adhoc,            0}, /* :experimental */
    {P("alarm"),            p_timerEvent,       0}, /* :replaces timer; one-shot sig fire */
    {P("args"),             p_args,             0},
    {P("argv"),             p_mainArgs,         0},
    {P("clone"),            p_new,              0}, /* :replaces new, this is obviously cloning op */
    {P("clonesym"),         p_defineNew,        0}, /* :replaces def-new -||- */
    {P("commandevent"),     p_commandEvent,     0},
    {P("context"),          p_context,          0},
    {P("copy"),             p_copy,             0},
    {P("delete"),           p_deleteSymbol,     0},
    {P("dump"),             p_dump,             0},
    {P("dumpsym"),          p_dumpSymbol,       0}, /* :debug not documented in original */
    {P("env"),              p_env,              0},
    {P("errorevent"),       p_errorEvent,       0},
    {P("etime"),            p_time,             0}, /* :replaces time, measures elapsed eval time */
    {P("eval"),             p_eval,             0},
    {P("evalstr"),          p_evalString,       0},
    {P("exit"),             p_exit,             0},
    {P("functor"),          p_default,          0}, /* :replaces default, returns value of default functor */
    {P("history"),          p_history,          0},
    {P("lasterr"),          p_lastError,        0},
    {P("load"),             p_load,             0},
    {P("locale"),           p_setLocale,        0},
    {P("mstimeday"),        p_timeOfDay,        0}, /* :replaces time-of-day, high-res time in ms */
    {P("mstime"),           p_timeId,           0}, /* :rebel */
    {P("pprint"),           p_prettyPrint,      0},
    {P("prefix"),           p_prefix,           0},
    {P("promptevent"),      p_promptEvent,      0},
    {P("quote"),            p_quote,            0},
    {P("readerevent"),      p_readerEvent,      0},
    {P("readexpr"),         p_readExpr,         0},
    {P("reset"),            p_reset,            0},
    {P("save"),             p_save,             0},
    {P("silent"),           p_silent,           0},
    {P("sleep"),            p_sleep,            0},
    {P("source"),           p_symbolSource,     0},
    {P("sym"),              p_symbol,           0},
    {P("symbols"),          p_symbols,          0},
    {P("syserr"),           p_systemError,      0},
    {P("sysinfo"),          p_systemInfo,       0},
    {P("term"),             p_term,             0},
    {P("throwerror"),       p_throwError,       0},
    {P("error"),            p_throwError,       0}, /* :alt */
    {P("trace"),            p_trace,            0},
    {P("xferevent"),        p_transferEvent,    0},

    #ifdef DEBUGGER
    {P("debug"),            p_debug,            0},
    {P("tracemarks"),       p_traceHighlight,   0}, /* :replaces trace-highlight */
    #endif

    /* core - C */

    {P("address"),          p_address,          0},
    {P("callback"),         p_callback,         0},
    {P("flt"),              p_flt,              0},
    {P("charc"),            p_getChar,          0},
    {P("floatc"),           p_getFloat,         0},
    {P("intc"),             p_getInteger,       0},
    {P("longc"),            p_getLong,          0},
    {P("stringc"),          p_getString,        0},
    {P("import"),           p_importLib,        0},
    {P("memcpy"),           p_copyMemory,       0}, /* :replaces cpymem */
    {P("pack"),             p_pack,             0},
    #ifdef FFI
    {P("struct"),           p_struct,           0},
    #endif
    {P("unpack"),           p_unpack,           0},

    /* core - predicates */

    {P("nan?"),             p_isnan,            0},
    {P("array?"),           p_isArray,          0},
    {P("atom?"),            p_isAtom,           0},
    {P("bigint?"),          p_isBigInteger,     0},
    {P("context?"),         p_isContext,        0},
    {P("dir?"),             p_isDirectory,      0},
    {P("empty?"),           p_isEmpty,          0},
    {P("even?"),            p_isEven,           0},
    {P("file?"),            p_isFile,           0},
    {P("float?"),           p_isFloat,          0},
    {P("fn?"),              p_isFn,             0},
    {P("inf?"),             p_isinf,            0},
    {P("int?"),             p_isInteger,        0}, /* :replaces integer? */
    {P("internal?"),        p_isInternal,       0}, /* :rebel */
    {P("legal?"),           p_isLegal,          0},
    {P("list?"),            p_isList,           0},
    {P("local?"),           p_isLocal,          0}, /* :rebel */
    {P("macro?"),           p_isMacro,          0},
    {P("nil?"),             p_isNil,            0},
    {P("null?"),            p_isNull,           0},
    {P("number?"),          p_isNumber,         0},
    {P("odd?"),             p_isOdd,            0},
    {P("primitive?"),       p_isPrimitive,      0},
    {P("protected?"),       p_isProtected,      0},
    {P("quote?"),           p_isQuote,          0},
    {P("set?"),             p_isSet,            0}, /* :rebel */
    {P("shared?"),          p_isShared,         0}, /* :replaces global? */
    {P("string?"),          p_isString,         0},
    {P("symbol?"),          p_isSymbol,         0},
    {P("true?"),            p_isTrue,           0},
    {P("zero?"),            p_isZero,           0},

    /* core - flow - code reading boosters */

    {P("is"),               p_equal,            0}, /* :alt for = */
    {P("ok"),               p_isTrue,           0}, /* :alt, explicit boolean in some cases */
    {P("no"),               p_isNull,           0}, /* :alt, sentinel of usability in some cases. covers nil?,null?,empty?,zero?,nan? */
    {P("ret"),              p_ret,              2}, /* :rebel */
    /* {P("hash"),             NULL,               0}, */

    /* core - date and time */

    {P("date"),             p_date,             0}, /* date as human-readable string */
    {P("dateiso"),          p_dateISO,          0}, /* :rebel date as ISO 8601 string */
    {P("datelist"),         p_dateList,         0}, /* date and time as list */
    {P("datestamp"),        p_dateParse,        0}, /* :replaces date-parse, seconds since epoch to date */
    {P("time"),             p_dateValue,        0}, /* :replaces date-value, seconds since epoch (UTC); adjustable */
    {P("timelist"),         p_now,              0}, /* :replaces now, current date and time +/- sec offset as list */

    /* core - network */

    {P("netclose"),           p_netClose,         0},
    {P("netservice"),         p_netService,       0},
    {P("netconnect"),         p_netConnect,       0},
    {P("netaccept"),          p_netAccept,        0},
    {P("netlocal"),           p_netLocal,         0},
    {P("netpeer"),            p_netPeer,          0},
    {P("netipv"),             p_netIpv,           0},
    {P("netlookup"),          p_netLookup,        0},
    {P("netrecv"),            p_netReceive,       0x400},
    {P("netrecvfrom"),        p_netReceiveFrom,   0},
    {P("netrecvudp"),         p_netReceiveUDP,    0},
    {P("netsend"),            p_netSend,          0},
    {P("netsendto"),          p_netSendTo,        0},
    {P("netsendudp"),         p_netSendUDP,       0},
    {P("netlisten"),          p_netListen,        0},
    {P("netpacket"),          p_netPacket,        0},
    {P("netping"),            p_netPing,          0},
    {P("netpeek"),            p_netPeek,          0},
    {P("netselect"),          p_netSelect,        0},
    {P("netsessions"),        p_netSessions,      0},
    {P("neteval"),            p_netEval,          0},
    {P("netinterface"),       p_netInterface,     0},
    {P("neterr"),             p_netLastError,     0},

    /* core - http */

    {P("urlget"),             p_getUrl,           0}, /* :replaces get-url */
    {P("urlput"),             p_putUrl,           0}, /* :replaces put-url */
    {P("urlpost"),            p_postUrl,          0}, /* :replaces post-url */
    {P("urldel"),             p_deleteUrl,        0}, /* :replaces delete-url */


    /* NON-CORE - science/domain specific -------------------------- */

    #ifdef NON_CORE

    /* non-core - matrix */

    {P("mat.apply"),            p_matScalar,        0}, /* :replaces mat */
    {P("mat.det"),              p_determinant,      0},
    {P("mat.invert"),           p_matInvert,        0},
    {P("mat.multiply"),         p_matMultiply,      0},
    {P("mat.transpose"),        p_matTranspose,     0},

    /* non-core - statistics */

    {P("sta.bayes-query"),      p_bayesQuery,       0},
    {P("sta.bayes-train"),      p_bayesTrain,       0},
    {P("sta.beta"),             p_beta,             0},
    {P("sta.betai"),            p_betai,            0},
    {P("sta.binomial"),         p_binomial,         0},
    {P("sta.corr"),             p_corr,             0},
    {P("sta.crit-chi2"),        p_criticalChi2,     0},
    {P("sta.crit-f"),           p_criticalF,        0},
    {P("sta.crit-t"),           p_criticalT,        0},
    {P("sta.crit-z"),           p_criticalZ,        0},
    {P("sta.fft"),              p_fft,              0},
    {P("sta.gammai"),           p_gammai,           0},
    {P("sta.gammaln"),          p_gammaln,          0},
    {P("sta.ifft"),             p_ifft,             0},
    {P("sta.kmeans-query"),     p_kmeansQuery,      0},
    {P("sta.kmeans-train"),     p_kmeansTrain,      0},
    {P("sta.prob-chi2"),        p_probabilityChi2,  0},
    {P("sta.prob-f"),           p_probabilityF,     0},
    {P("sta.prob-t"),           p_probabilityT,     0},
    {P("sta.prob-z"),           p_probabilityZ,     0},
    {P("sta.stats"),            p_stats,            0},
    {P("sta.t-test"),           p_ttest,            0},

    /* non-core - finantial math */

    {P("fin.fv"),               p_fv,               0},
    {P("fin.irr"),              p_irr,              0},
    {P("fin.nper"),             p_nper,             0},
    {P("fin.npv"),              p_npv,              0},
    {P("fin.pmt"),              p_pmt,              0},
    {P("fin.pv"),               p_pv,               0},

    /* non-core - encoding */

    #ifdef XML_SUPPORT
    {P("enc.xml-error"),        p_XMLerror,         0},
    {P("enc.xml-parse"),        p_XMLparse,         0},
    {P("enc.xml-type-tags"),    p_XMLtypeTags,      0},
    #endif
    {P("enc.json-error"),       p_JSONerror,        0},
    {P("enc.json-parse"),       p_JSONparse,        0},
    #endif /* NON_CORE */

    {NULL,NULL,0},
};

#endif /* PRIMES_H */
