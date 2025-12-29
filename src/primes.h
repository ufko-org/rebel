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

    (alias 'bt sta-bayes-train) for current context
    (alias (global 'bt) sta-bayes-train) for all contexts

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

       noun          - string            (conversion operation) 
       action        - rotate            (generic operation)
       nounaction    - stringrotate      (specific operation)
       value         - crc32             (computed value / algorithm name)
       actionnoun    - dumpsym           (the worst)

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
    false?   -> no, ko

    primes.h is the authoritative source.
*/

#ifndef PRIMES_H
#define PRIMES_H

PRIMITIVE primitive[] =
{
    /* CORE ------------------------------------------------------------- */

    /* core - data - creators */

    {"constant",         p_constant,         0x400},
    {"const",            p_constant,         0x400}, /* :alt */
    {"alias",            p_constant,         0x400}, /* :alt */
    {"global",           p_global,           0},
    {"let",              p_let,              0x402},
    {"letex",            p_letExpand,        0x403},
    {"letn",             p_letn,             0x002},
    {"local",            p_local,            2},
    {"mac",              p_defineMacro,      0x402},
    {"macro",            p_macro,            2},
    {"def",              p_define,           0x402},
    {"set",              p_setdef,           0x400}, /* :rebel */ 
    {"with",             p_with,             0x400}, /* :rebel */

    /* core - data - iterators */
    
    {"doargs",           p_doargs,           2},
    {"dolist",           p_dolist,           2},
    {"dostring",         p_dostring,         2},
    {"dotree",           p_dotree,           2},

    /* core - data - sequencers */

    {"array",            p_array,            0},
    {"list",             p_list,             0},
    {"sequence",         p_sequence,         0},
    {"series",           p_series,           0},

    /* core - data - convertors/extractors */

    {"arraylist",        p_arrayList,        0},
    {"bigint",           p_bigInt,           0},
    {"bits",             p_bits,             0},
    {"char",             p_char,             0},
    {"float",            p_float,            0},
    {"int",              p_integer,          0},
    {"string",           p_string,           0},
    {"sym",              p_symbol,           0}, /* value of symbol */
    {"functor",          p_default,          0}, /* value of default functor */

    /* core - data - transformers */

    {"b64dec",           p_base64Dec,        0},
    {"b64enc",           p_base64Enc,        0},
    {"crc32",            p_crc32,            0},
    {"encrypt",          p_encrypt,          0},
    {"uuid",             p_uuid,             0},

    /* core - data - workers */

    {"append",           p_append,           0},
    {"apply",            p_apply,            0},
    {"assoc",            p_assoc,            0},
    {"bind",             p_bind,             0x400},
    {"chop",             p_chop,             0},
    {"clean",            p_clean,            0},
    {"collect",          p_collect,          0},
    {"cons",             p_cons,             0},
    {"curry",            p_curry,            0},
    {"freq",             p_count,            0}, /* freq, not the count */
    {"difference",       p_difference,       0},
    {"dup",              p_dup,              0},
    {"exists",           p_exists,           0}, /* misleading name. looks like predicate, returns value/nil */
    {"pickif",           p_exists,           0}, /* :alt */
    {"expand",           p_expand,           0},
    {"explode",          p_explode,          0},
    {"extend",           p_extend,           0x400},
    {"filter",           p_filter,           0},
    {"find",             p_find,             0},
    {"findall",          p_findAll,          0},
    {"first",            p_first,            0},
    {"flat",             p_flat,             0},
    {"fmt",              p_format,           0},
    {"index",            p_index,            0},
    {"intersect",        p_intersect,        0},
    {"join",             p_join,             0},
    {"last",             p_last,             0},
    {"lcase",            p_lower,            0},
    {"length",           p_length,           0},
    {"lookup",           p_lookup,           0},
    {"map",              p_map,              0},
    {"match",            p_match,            0},
    {"member",           p_member,           0},
    {"nth",              p_nth,              0},
    {"parse",            p_parse,            0},
    {"pop",              p_pop,              0x400},
    {"popassoc",         p_popAssoc,         0x400},
    {"push",             p_push,             0x400},
    {"ref",              p_ref,              0},
    {"refall",           p_refAll,           0},
    {"refset",           p_setRef,           0x400},
    {"refsetall",        p_setRefAll,        0x400},
    {"replace",          p_replace,          0x400},
    {"rest",             p_rest,             0},
    {"reverse",          p_reverse,          0x400},
    {"rotate",           p_rotate,           0x400},
    {"rx",               p_regex,            0},
    {"rxcomp",           p_regexComp,        0},
    {"select",           p_select,           0},
    {"slice",            p_slice,            0},
    {"sort",             p_sort,             0x400},
    {"swap",             p_swap,             0},
    {"tcase",            p_title,            0},
    {"throw",            p_throw,            0},
    {"trim",             p_trim,             0},
    {"ucase",            p_upper,            0},
    {"unify",            p_unify,            0},
    {"union",            p_union,            0},
    {"unique",           p_unique,           0},

    #ifdef SUPPORT_UTF8
    {"utf8len",          p_utf8len,          0},
    #endif

    /* core - math - int */

    {"+",                p_add,              0},
    {"-",                p_subtract,         0},
    {"*",                p_multiply,         0},
    {"/",                p_divide,           0},
    {"%",                p_modulo,           0},
    {"++",               p_incrementI,       0x400},
    {"--",               p_decrementI,       0x400},

    /* core - math - float */

    {"add",              p_addFloat,         0},
    {"sub",              p_subFloat,         0},
    {"mul",              p_mulFloat,         0},
    {"div",              p_divFloat,         0},
    {"mod",              p_modFloat,         0}, 
    {"inc",              p_incrementF,       0x400},
    {"dec",              p_decrementF,       0x400},

    /* core - math - float extended */

    {"abs",              p_abs,              0},
    {"acos",             p_acos,             0},
    {"acosh",            p_acosh,            0},
    {"asin",             p_asin,             0},
    {"asinh",            p_asinh,            0},
    {"atan",             p_atan,             0},
    {"atan2",            p_atan2,            0},
    {"atanh",            p_atanh,            0},
    {"ceil",             p_ceil,             0},
    {"cos",              p_cos,              0},
    {"cosh",             p_cosh,             0},
    {"erf",              p_erf,              0},
    {"exp",              p_exp,              0},
    {"factor",           p_factor,           0},
    {"floor",            p_floor,            0},
    {"gcd",              p_gcd,              0},
    {"log",              p_log,              0},
    {"max",              p_maxFloat,         0},
    {"min",              p_minFloat,         0},
    {"pow",              p_powFloat,         0},
    {"round",            p_round,            0},
    {"sgn",              p_sgn,              0},
    {"sin",              p_sin,              0},
    {"sinh",             p_sinh,             0},
    {"sqrt",             p_sqrt,             0},
    {"ssq",              p_ssq,              0},
    {"tan",              p_tan,              0},
    {"tanh",             p_tanh,             0},

    /* core - flow */

    {"do",               p_evalBlock,        1}, /* begin */
    {"silent",           p_silent,           0},
    {"case",             p_case,             2},
    {"cond",             p_condition,        1},
    {"dountil",          p_doUntil,          2},
    {"dowhile",          p_doWhile,          2},
    {"dotimes",          p_dotimes,          2},
    {"for",              p_for,              2},
    {"if",               p_if,               2},
    {"unless",           p_unless,           2},
    {"until",            p_until,            2},
    {"when",             p_when,             2},
    {"while",            p_while,            2},

    /* core - logical */

    {"and",              p_and,              0},
    {"or",               p_or,               0},
    {"not",              p_not,              0},

    /* core - comparison ops */

    {"<",                p_less,             0},
    {"lt",               p_less,             0}, /* :alt */
    {">",                p_greater,          0},
    {"gt",               p_greater,          0}, /* :alt */
    {"<=",               p_lessEqual,        0},
    {"le",               p_lessEqual,        0}, /* :alt */
    {">=",               p_greaterEqual,     0},
    {"ge",               p_greaterEqual,     0}, /* :alt */
    {"=",                p_equal,            0},
    {"eq",               p_equal,            0}, /* :alt */
    {"!=",               p_notEqual,         0},
    {"ne",               p_notEqual,         0}, /* :alt */

    /* core - bit ops */

    {"<<",               p_shiftLeft,        0},
    {"shl",              p_shiftLeft,        0}, /* :alt */
    {">>",               p_shiftRight,       0},
    {"shr",              p_shiftRight,       0}, /* :alt */
    {"&",                p_bitAnd,           0},
    {"band",             p_bitAnd,           0}, /* :alt */
    {"|",                p_bitOr,            0},
    {"bor",              p_bitOr,            0}, /* :alt */
    {"^",                p_bitXor,           0},
    {"bxor",             p_bitXor,           0}, /* :alt */
    {"~",                p_bitNot,           0},
    {"bnot",             p_bitNot,           0}, /* :alt */

    /* core - random */

    {"amb",              p_amb,              0},
    {"pick",             p_amb,              0}, /* :alt */
    {"normal",           p_normal,           0},
    {"rand",             p_rand,             0},
    {"random",           p_random,           0},
    {"randomize",        p_randomize,        0},
    {"seed",             p_seed,             0},

    /* core - io - via std */

    {"pprint",           p_prettyPrint,      0},
    {"print",            p_print,            0},
    {"pr",               p_print,            0}, /* :alt */
    {"println",          p_println,          0},
    {"prn",              p_println,          0}, /* :alt */
    {"readkey",          p_readKey,          0}, /* user input */

    /* core - io - via device number */
    
    {"device",           p_device,           0},
    {"open",             p_open,             0},
    {"close",            p_close,            0},
    {"seek",             p_seek,             0},
    #ifdef SUPPORT_UTF8
    {"readc8",           p_readUTF8,         0},
    #endif
    {"read",             p_readBuffer,       0x400}, 
    {"write",            p_writeBuffer,      0}, 
    {"readc",            p_readChar,         0},
    {"writec",           p_writeChar,        0},
    {"readln",           p_readLine,         0},
    {"writeln",          p_writeLine,        0},
    {"cline",            p_currentLine,      0},

    /* core - io - via path */

    {"fpath",            p_realpath,         0},
    {"finfo",            p_fileInfo,         0}, /* not all stat fields */
    {"fappend",          p_appendFile,       0},
    {"fread",            p_readFile,         0},
    {"fwrite",           p_writeFile,        0},
    {"fcopy",            p_copyFile,         0},
    {"fmove",            p_renameFile,       0},
    {"fdel",             p_deleteFile,       0},
    {"fgrep",            p_search,           0},
    
    /* core - io - directories */

    {"dir",              p_directory,        0},
    {"dirpath",          p_realpath,         0},
    {"dirmk",            p_makeDir,          0},
    {"dirrm",            p_removeDir,        0},
    {"dircd",            p_changeDir,        0},

    /* core - OS/CILK processes */

    {"!",                p_system,           0},
    {"run",              p_system,           0}, /* :alt */
    {"kill",             p_destroyProcess,   0}, /* this is Unix */
    {"exec",             p_exec,             0},
    {"process",          p_process,          0},
    {"pipe",             p_pipe,             0},
    {"fork",             p_fork,             0},
    {"waitpid",          p_waitpid,          0},
    {"spawn",            p_spawn,            0},
    {"sync",             p_sync,             0},
    {"abort",            p_abort,            0},
    {"psend",            p_send,             0},
    {"precv",            p_receive,          0},
    {"signal",           p_signal,           0},
    {"share",            p_share,            0},
    {"semaphore",        p_semaphore,        0},
    {"peek",             p_peek,             0},

    /* core - internals */

    {"$",                p_systemSymbol,     0},
    {":",                p_colon,            0},

    {"args",             p_args,             0},
    {"argv",             p_mainArgs,         0},
    {"catch",            p_catch,            0},
    {"commandevent",     p_commandEvent,     0},
    {"context",          p_context,          0},
    {"copy",             p_copy,             0},
    {"defnew",           p_defineNew,        0},
    {"cloneone",         p_defineNew,        0}, /* :alt */
    {"delete",           p_deleteSymbol,     0},
    {"dump",             p_dump,             0},
    {"dumpsymbol",       p_dumpSymbol,       0}, /* :debug not documented in original */
    {"env",              p_env,              0},
    {"errorevent",       p_errorEvent,       0},
    {"etime",            p_time,             0}, /* measures elapsed eval time like sh time */
    {"eval",             p_eval,             0},
    {"evalstr",          p_evalString,       0},
    {"exit",             p_exit,             0},
    {"history",          p_history,          0},
    {"lasterr",          p_lastError,        0},
    {"load",             p_load,             0},
    {"locale",           p_setLocale,        0},
    {"new",              p_new,              0},
    {"clone",            p_new,              0}, /* :alt this is obviously cloning op */
    {"prefix",           p_prefix,           0},
    {"promptevent",      p_promptEvent,      0},
    {"quote",            p_quote,            0},
    {"readerevent",      p_readerEvent,      0},
    {"readexpr",         p_readExpr,         0},
    {"reset",            p_reset,            0},
    {"save",             p_save,             0},
    {"self",             p_self,             0},
    {"source",           p_symbolSource,     0},
    {"symbols",          p_symbols,          0},
    {"syserr",           p_systemError,      0},
    {"sysinfo",          p_systemInfo,       0},
    {"term",             p_term,             0},
    {"throwerror",       p_throwError,       0},
    {"timer",            p_timerEvent,       0},
    {"trace",            p_trace,            0},
    {"xferevent",        p_transferEvent,    0},

    #ifdef DEBUGGER
    {"debug",            p_debug,            0},
    {"tracemark",        p_traceHighlight,   0},
    #endif

    /* core - C */
    
    {"address",          p_address,          0},
    {"callback",         p_callback,         0},
    {"flt",              p_flt,              0},
    {"charc",            p_getChar,          0},
    {"floatc",           p_getFloat,         0},
    {"intc",             p_getInteger,       0},
    {"longc",            p_getLong,          0},
    {"stringc",          p_getString,        0},
    {"import",           p_importLib,        0},
    {"memcpy",           p_copyMemory,       0},
    {"pack",             p_pack,             0},
    {"sleep",            p_sleep,            0},
    {"struct",           p_struct,           0},
    {"unpack",           p_unpack,           0},

    /* core - predicates */

    {"nan?",             p_isnan,            0},
    {"all?",             p_forAll,           0},
    {"any?",             p_any,              0}, /* :rebel */
    {"array?",           p_isArray,          0},
    {"atom?",            p_isAtom,           0},
    {"bigint?",          p_isBigInteger,     0},
    {"context?",         p_isContext,        0},
    {"dir?",             p_isDirectory,      0},
    {"empty?",           p_isEmpty,          0},
    {"ends?",            p_endsWith,         0},
    {"even?",            p_isEven,           0},
    {"file?",            p_isFile,           0},
    {"float?",           p_isFloat,          0},
    {"fn?",              p_isFn,             0},
    {"global?",          p_isGlobal,         0},
    {"inf?",             p_isinf,            0},
    {"integer?",         p_isInteger,        0},
    {"legal?",           p_isLegal,          0},
    {"list?",            p_isList,           0},
    {"macro?",           p_isMacro,          0},
    {"nil?",             p_isNil,            0},
    {"null?",            p_isNull,           0},
    {"number?",          p_isNumber,         0},
    {"odd?",             p_isOdd,            0},
    {"primitive?",       p_isPrimitive,      0},
    {"protected?",       p_isProtected,      0},
    {"quote?",           p_isQuote,          0},
    {"starts?",          p_startsWith,       0},
    {"string?",          p_isString,         0},
    {"symbol?",          p_isSymbol,         0},
    {"true?",            p_isTrue,           0},
    {"zero?",            p_isZero,           0},

    /* core - flow - intent markers */

    {"ok",               p_isTrue,           0}, /* :alt, explicit value -> boolean */

    /* core - date and time */

    {"date",             p_date,             0}, /* date as human-readable string */ 
    {"dateiso",          p_dateISO,          0}, /* :rebel date as ISO8601 */ 
    {"datelist",         p_dateList,         0}, /* date time as list */
    {"datestamp",        p_dateParse,        0}, /* seconds since epoch to date */
    {"time",             p_dateValue,        0}, /* seconds since epoch (UTC) */ 
    {"timelist",         p_now,              0}, /* curent date time +/-sec offset as list */
    {"timeofday",        p_timeOfDay,        0}, /* seconds elapsed since midnight */

    /* core - network */

    {"nclose",           p_netClose,         0},
    {"nservice",         p_netService,       0},
    {"nconnect",         p_netConnect,       0},
    {"naccept",          p_netAccept,        0},
    {"nlocal",           p_netLocal,         0},
    {"npeer",            p_netPeer,          0},
    {"nipv",             p_netIpv,           0},
    {"nlookup",          p_netLookup,        0},
    {"nrecv",            p_netReceive,       0x400},
    {"nrecvfrom",        p_netReceiveFrom,   0},
    {"nrecvudp",         p_netReceiveUDP,    0},
    {"nsend",            p_netSend,          0},
    {"nsendto",          p_netSendTo,        0},
    {"nsendudp",         p_netSendUDP,       0},
    {"nlisten",          p_netListen,        0},
    {"npacket",          p_netPacket,        0},
    {"nping",            p_netPing,          0},
    {"npeek",            p_netPeek,          0},
    {"nselect",          p_netSelect,        0},
    {"nsessions",        p_netSessions,      0},
    {"neval",            p_netEval,          0},
    {"ninterface",       p_netInterface,     0},
    {"nlasterr",         p_netLastError,     0},

    /* core - http */

    {"hget",             p_getUrl,           0},
    {"hput",             p_putUrl,           0},
    {"hpost",            p_postUrl,          0},
    {"hdel",             p_deleteUrl,        0},

  
    /* NON-CORE - not needed for my daily work -------------------------- */

    #ifdef NON_CORE

    /* non-core - matrix */

    {"mat-transpose",        p_matTranspose,     0},
    {"mat-multiply",         p_matMultiply,      0},
    {"mat-invert",           p_matInvert,        0},
    {"mat-det",              p_determinant,      0},
    {"mat-apply",            p_matScalar,        0},

    /* non-core - statistics */

    {"sta-bayes-train",      p_bayesTrain,       0},
    {"sta-bayes-query",      p_bayesQuery,       0},
    {"sta-kmeans-train",     p_kmeansTrain,      0},
    {"sta-kmeans-query",     p_kmeansQuery,      0},
    {"sta-stats",            p_stats,            0},
    {"sta-t-test",           p_ttest,            0},
    {"sta-corr",             p_corr,             0},
    {"sta-prob-z",           p_probabilityZ,     0},
    {"sta-prob-chi2",        p_probabilityChi2,  0},
    {"sta-prob-t",           p_probabilityT,     0},
    {"sta-prob-f",           p_probabilityF,     0},
    {"sta-crit-chi2",        p_criticalChi2,     0},
    {"sta-crit-z",           p_criticalZ,        0},
    {"sta-crit-t",           p_criticalT,        0},
    {"sta-crit-f",           p_criticalF,        0},
    {"sta-fft",              p_fft,              0},
    {"sta-ifft",             p_ifft,             0},
    {"sta-beta",             p_beta,             0},
    {"sta-betai",            p_betai,            0},
    {"sta-gammaln",          p_gammaln,          0},
    {"sta-gammai",           p_gammai,           0},
    {"sta-binomial",         p_binomial,         0},

    /* non-core - finantial math */

    {"fin-pmt",              p_pmt,              0},
    {"fin-pv",               p_pv,               0},
    {"fin-fv",               p_fv,               0},
    {"fin-nper",             p_nper,             0},
    {"fin-npv",              p_npv,              0},
    {"fin-irr",              p_irr,              0},
      
    /* non-core - encoding */

    #ifdef XML_SUPPORT
    {"enc-xml-parse",        p_XMLparse,         0},
    {"enc-xml-error",        p_XMLerror,         0},
    {"enc-xml-type-tags",    p_XMLtypeTags,      0},
    #endif
    {"enc-json-parse",       p_JSONparse,        0},
    {"enc-json-error",       p_JSONerror,        0},

    #endif /* NON_CORE */

    {NULL,NULL,0},
};

#endif /* PRIMES_H */
