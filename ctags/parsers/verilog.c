/*
*   Copyright (c) 2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for the Verilog HDL
*   (Hardware Description Language).
*
*   Language definition documents:
*       http://www.eg.bucknell.edu/~cs320/verilog/verilog-manual.html
*       http://www.sutherland-hdl.com/on-line_ref_guide/vlog_ref_top.html
*       http://www.verilog.com/VerilogBNF.html
*       http://eesun.free.fr/DOC/VERILOG/verilog_manual1.html
*   Also [1] IEEE Std 1800-2012 (can be found on the Internet as a PDF file)
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>
#include <setjmp.h>

#include "debug.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "lcpp.h"
#include "routines.h"
#include "xtag.h"

/*
 *   DATA DECLARATIONS
 */
typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

typedef enum {
	K_UNDEFINED = -1,
	K_CONSTANT,
	K_MACRO,
	K_EVENT,
	K_FUNCTION,
	K_MODULE,
	K_NET,
	K_PORT,
	K_REGISTER,
	K_TASK,
	K_LOGIC,
	K_ENUM,
	K_TYPEDEF,
	K_STRUCT,
	K_UNION,
	K_PACKAGE,
	K_OTHER
} verilogKind;

/*
 *   DATA DEFINITIONS
 */
static int Ungetc;
static int Lang_verilog;
static jmp_buf Exception;

static kindDefinition VerilogKinds [] = {
 { true, 'c', "constant",  "constants (define, parameter, specparam)" },
 { true, 'a', "macro",     "macros" },
 { true, 'e', "event",     "events" },
 { true, 'f', "function",  "functions" },
 { true, 'm', "module",    "modules" },
 { true, 'n', "net",       "net data types" },
 { true, 'p', "port",      "ports" },
 { true, 'r', "register",  "register data types" },
 { true, 't', "task",      "tasks" },
 { true, 'l', "logic",     "generalized logic data types" },
 { true, 'w', "enum",      "enumerations" },
 { true, 'd', "typedef",   "typedefs" },
 { true, 's', "struct",    "structs" },
 { true, 'u', "union",     "unions" },
 { true, 'g', "package",   "packages" },
 { true, 'o', "other",     "other language constructs" }
};

static keywordTable VerilogKeywordTable [] = {
	{ "`define",   K_MACRO },
	{ "typedef",   K_TYPEDEF },
	{ "genvar",    K_CONSTANT },
	{ "event",     K_EVENT },
	{ "function",  K_FUNCTION },
	{ "inout",     K_PORT },
	{ "input",     K_PORT },
	{ "integer",   K_REGISTER },
	{ "module",    K_MODULE },
	{ "output",    K_PORT },
	{ "parameter", K_CONSTANT },
	{ "localparam", K_CONSTANT },
	{ "real",      K_REGISTER },
	{ "realtime",  K_REGISTER },
	{ "reg",       K_REGISTER },
	{ "specparam", K_CONSTANT },
	{ "supply0",   K_NET },
	{ "supply1",   K_NET },
	{ "task",      K_TASK },
	{ "time",      K_REGISTER },
	{ "tri0",      K_NET },
	{ "tri1",      K_NET },
	{ "triand",    K_NET },
	{ "tri",       K_NET },
	{ "trior",     K_NET },
	{ "trireg",    K_NET },
	{ "wand",      K_NET },
	{ "wire",      K_NET },
	{ "wor",       K_NET },
	{ "logic",     K_LOGIC },
	{ "byte",      K_LOGIC },
	{ "shortint",  K_LOGIC },
	{ "int",       K_LOGIC },
	{ "longint",   K_LOGIC },
	{ "bit",       K_LOGIC },
	{ "enum",      K_ENUM },
	{ "struct",    K_STRUCT },
	{ "union",     K_UNION },
	{ "package",   K_PACKAGE },
	//
	// All the remaining SystemVerilog keywords (see [1], p.1145)
	{ "accept_on",           K_OTHER },
	{ "alias",               K_OTHER },
	{ "always_comb",         K_OTHER },
	{ "always_ff",           K_OTHER },
	{ "always",              K_OTHER },
	{ "always_latch",        K_OTHER },
	{ "and",                 K_OTHER },
	{ "assert",              K_OTHER },
	{ "assign",              K_OTHER },
	{ "assume",              K_OTHER },
	{ "automatic",           K_OTHER },
	{ "before",              K_OTHER },
	{ "begin",               K_OTHER },
	{ "bind",                K_OTHER },
	{ "bins",                K_OTHER },
	{ "binsof",              K_OTHER },
	{ "break",               K_OTHER },
	{ "bufif0",              K_OTHER },
	{ "bufif1",              K_OTHER },
	{ "buf",                 K_OTHER },
	{ "case",                K_OTHER },
	{ "casex",               K_OTHER },
	{ "casez",               K_OTHER },
	{ "cell",                K_OTHER },
	{ "chandle",             K_OTHER },
	{ "checker",             K_OTHER },
	{ "class",               K_OTHER },
	{ "clocking",            K_OTHER },
	{ "cmos",                K_OTHER },
	{ "config",              K_OTHER },
	{ "const",               K_OTHER },
	{ "constraint",          K_OTHER },
	{ "context",             K_OTHER },
	{ "continue",            K_OTHER },
	{ "covergroup",          K_OTHER },
	{ "cover",               K_OTHER },
	{ "coverpoint",          K_OTHER },
	{ "cross",               K_OTHER },
	{ "deassign",            K_OTHER },
	{ "default",             K_OTHER },
	{ "defparam",            K_OTHER },
	{ "design",              K_OTHER },
	{ "disable",             K_OTHER },
	{ "dist",                K_OTHER },
	{ "do",                  K_OTHER },
	{ "edge",                K_OTHER },
	{ "else",                K_OTHER },
	{ "endcase",             K_OTHER },
	{ "endchecker",          K_OTHER },
	{ "endclass",            K_OTHER },
	{ "endclocking",         K_OTHER },
	{ "endconfig",           K_OTHER },
	{ "endfunction",         K_OTHER },
	{ "endgenerate",         K_OTHER },
	{ "endgroup",            K_OTHER },
	{ "endinterface",        K_OTHER },
	{ "end",                 K_OTHER },
	{ "endmodule",           K_OTHER },
	{ "endpackage",          K_OTHER },
	{ "endprimitive",        K_OTHER },
	{ "endprogram",          K_OTHER },
	{ "endproperty",         K_OTHER },
	{ "endsequence",         K_OTHER },
	{ "endspecify",          K_OTHER },
	{ "endtable",            K_OTHER },
	{ "endtask",             K_OTHER },
	{ "eventually",          K_OTHER },
	{ "expect",              K_OTHER },
	{ "export",              K_OTHER },
	{ "extends",             K_OTHER },
	{ "extern",              K_OTHER },
	{ "final",               K_OTHER },
	{ "first_match",         K_OTHER },
	{ "force",               K_OTHER },
	{ "foreach",             K_OTHER },
	{ "forever",             K_OTHER },
	{ "forkjoin",            K_OTHER },
	{ "fork",                K_OTHER },
	{ "for",                 K_OTHER },
	{ "generate",            K_OTHER },
	{ "global",              K_OTHER },
	{ "highz0",              K_OTHER },
	{ "highz1",              K_OTHER },
	{ "iff",                 K_OTHER },
	{ "if",                  K_OTHER },
	{ "ifnone",              K_OTHER },
	{ "ignore_bins",         K_OTHER },
	{ "illegal_bins",        K_OTHER },
	{ "implements",          K_OTHER },
	{ "implies",             K_OTHER },
	{ "import",              K_OTHER },
	{ "incdir",              K_OTHER },
	{ "include",             K_OTHER },
	{ "initial",             K_OTHER },
	{ "inside",              K_OTHER },
	{ "instance",            K_OTHER },
	{ "interconnect",        K_OTHER },
	{ "interface",           K_OTHER },
	{ "intersect",           K_OTHER },
	{ "join_any",            K_OTHER },
	{ "join",                K_OTHER },
	{ "join_none",           K_OTHER },
	{ "large",               K_OTHER },
	{ "let",                 K_OTHER },
	{ "liblist",             K_OTHER },
	{ "library",             K_OTHER },
	{ "local",               K_OTHER },
	{ "macromodule",         K_OTHER },
	{ "matches",             K_OTHER },
	{ "medium",              K_OTHER },
	{ "modport",             K_OTHER },
	{ "nand",                K_OTHER },
	{ "negedge",             K_OTHER },
	{ "nettype",             K_OTHER },
	{ "new",                 K_OTHER },
	{ "nexttime",            K_OTHER },
	{ "nmos",                K_OTHER },
	{ "nor",                 K_OTHER },
	{ "noshowcancelled",     K_OTHER },
	{ "notif0",              K_OTHER },
	{ "notif1",              K_OTHER },
	{ "not",                 K_OTHER },
	{ "null",                K_OTHER },
	{ "or",                  K_OTHER },
	{ "packed",              K_OTHER },
	{ "pmos",                K_OTHER },
	{ "posedge",             K_OTHER },
	{ "primitive",           K_OTHER },
	{ "priority",            K_OTHER },
	{ "program",             K_OTHER },
	{ "property",            K_OTHER },
	{ "protected",           K_OTHER },
	{ "pull0",               K_OTHER },
	{ "pull1",               K_OTHER },
	{ "pulldown",            K_OTHER },
	{ "pullup",              K_OTHER },
	{ "pulsestyle_ondetect", K_OTHER },
	{ "pulsestyle_onevent",  K_OTHER },
	{ "pure",                K_OTHER },
	{ "randcase",            K_OTHER },
	{ "randc",               K_OTHER },
	{ "rand",                K_OTHER },
	{ "randsequence",        K_OTHER },
	{ "rcmos",               K_OTHER },
	{ "ref",                 K_OTHER },
	{ "reject_on",           K_OTHER },
	{ "release",             K_OTHER },
	{ "repeat",              K_OTHER },
	{ "restrict",            K_OTHER },
	{ "return",              K_OTHER },
	{ "rnmos",               K_OTHER },
	{ "rpmos",               K_OTHER },
	{ "rtranif0",            K_OTHER },
	{ "rtranif1",            K_OTHER },
	{ "rtran",               K_OTHER },
	{ "s_always",            K_OTHER },
	{ "scalared",            K_OTHER },
	{ "sequence",            K_OTHER },
	{ "s_eventually",        K_OTHER },
	{ "shortreal",           K_OTHER },
	{ "showcancelled",       K_OTHER },
	{ "signed",              K_OTHER },
	{ "small",               K_OTHER },
	{ "s_nexttime",          K_OTHER },
	{ "soft",                K_OTHER },
	{ "solve",               K_OTHER },
	{ "specify",             K_OTHER },
	{ "static",              K_OTHER },
	{ "string",              K_OTHER },
	{ "strong0",             K_OTHER },
	{ "strong1",             K_OTHER },
	{ "strong",              K_OTHER },
	{ "s_until",             K_OTHER },
	{ "s_until_with",        K_OTHER },
	{ "super",               K_OTHER },
	{ "sync_accept_on",      K_OTHER },
	{ "sync_reject_on",      K_OTHER },
	{ "table",               K_OTHER },
	{ "tagged",              K_OTHER },
	{ "this",                K_OTHER },
	{ "throughout",          K_OTHER },
	{ "timeprecision",       K_OTHER },
	{ "timeunit",            K_OTHER },
	{ "tranif0",             K_OTHER },
	{ "tranif1",             K_OTHER },
	{ "tran",                K_OTHER },
	{ "type",                K_OTHER },
	{ "unique0",             K_OTHER },
	{ "unique",              K_OTHER },
	{ "unsigned",            K_OTHER },
	{ "until",               K_OTHER },
	{ "until_with",          K_OTHER },
	{ "untyped",             K_OTHER },
	{ "use",                 K_OTHER },
	{ "uwire",               K_OTHER },
	{ "var",                 K_OTHER },
	{ "vectored",            K_OTHER },
	{ "virtual",             K_OTHER },
	{ "void",                K_OTHER },
	{ "wait",                K_OTHER },
	{ "wait_order",          K_OTHER },
	{ "weak0",               K_OTHER },
	{ "weak1",               K_OTHER },
	{ "weak",                K_OTHER },
	{ "while",               K_OTHER },
	{ "wildcard",            K_OTHER },
	{ "within",              K_OTHER },
	{ "with",                K_OTHER },
	{ "xnor",                K_OTHER },
	{ "xor",                 K_OTHER }
};

/*
 *   FUNCTION DEFINITIONS
 */

static void initialize (const langType language)
{
	size_t i;
	const size_t count = ARRAY_SIZE (VerilogKeywordTable);
	Lang_verilog = language;
	for (i = 0  ;  i < count  ;  ++i)
	{
		const keywordTable* const p = &VerilogKeywordTable [i];
		addKeyword (p->name, language, (int) p->id);
	}
}


// A convenience function to query keyword table for out parser's keywords
static verilogKind lookupParserKeyword(vString *const name){
	return (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
}


// Checks, if a string is a known keyword
static bool checkIsKnownKeyword(vString *const name){
	return K_UNDEFINED != lookupParserKeyword(name);
}


static void vUngetc (int c)
{
	Assert (Ungetc == '\0');
	Ungetc = c;
}

static int vGetc (void)
{
	// TODO: consider stripping (* ... *) attributes (pragma-like, may attach to anything)
	int c;
	if (Ungetc == '\0')
		c = getcFromInputFile ();
	else
	{
		c = Ungetc;
		Ungetc = '\0';
	}
	if (c == '/')
	{
		int c2 = getcFromInputFile ();
		if (c2 == EOF)
			longjmp (Exception, (int) ExceptionEOF);
		else if (c2 == '/')  /* strip comment until end-of-line */
		{
			do
				c = getcFromInputFile ();
			while (c != '\n'  &&  c != EOF);
		}
		else if (c2 == '*')  /* strip block comment */
		{
			c = cppSkipOverCComment();
		}
		else
		{
			ungetcToInputFile (c2);
		}
	}
	else if (c == '"')  /* strip string contents */
	{
		int c2;
		do
			c2 = getcFromInputFile ();
		while (c2 != '"'  &&  c2 != EOF);
		c = '@';
	}
	if (c == EOF)
		longjmp (Exception, (int) ExceptionEOF);
	return c;
}


// A subset of allowed characters to be in an unescaped identifier.
static bool isStandartAllowedIdentifierCharacter(const int c){
	return (bool)(isalnum (c)  ||  c == '_'  ||  c == '$');
}

// A convenience function, that intermixes identificators and macro for quick search
static bool isIdentifierCharacter (const int c)
{
	return (bool)(isStandartAllowedIdentifierCharacter(c) || c == '`');
}

/**
 * Checks if n contains a correct unescaped (System)Verilog identifier.
 * See "5.6 Identifiers, keywords, and system names", [1].
 */
static bool isAGoodIdentifier(vString const *const n){
	if(n -> length == 0) {
		return false;
	}
	size_t i;
	for(i=0; i < n -> length; i = i + 1){
		int c = n -> buffer[i];
		if(i == 0) {
			// Special checks for starting symbol
			if(isdigit(c) || c == '$') {
				return false;
			}
		}
		if(! isStandartAllowedIdentifierCharacter(c)){
			return false;
		}
	}
	return true;
}

static int skipWhite (int c)
{
	while (isspace (c))
		c = vGetc ();
	return c;
}

static int skipPastMatch (const char *const pair)
{
	const int begin = pair [0], end = pair [1];
	int matchLevel = 1;
	int c;
	do
	{
		c = vGetc ();
		if (c == begin)
			++matchLevel;
		else if (c == end)
			--matchLevel;
	}
	while (matchLevel > 0);
	return vGetc ();
}

static bool readIdentifier (vString *const name, int c)
{
	vStringClear (name);
	if (isIdentifierCharacter (c))
	{
		while (isIdentifierCharacter (c))
		{
			vStringPut (name, c);
			c = vGetc ();
		}
		vUngetc (c);
	}
	return (bool)(name->length > 0);
}

/**
 * An algorithm for detection of tags in different circumstances,
 * involving a list of tag[=something] with  some separators and an
 * escape condition.
 */
static void tagCommonAlgoNG(
	vString *const name,
	// Exit trigger brace sensitivity:
	bool sens_b,   // () simple
	bool sens_cb,  // {} curved
	bool sens_sqb, // [] square
	// Valid separators
	bool sep_comma,     // ','
	bool sep_semicolon, // ';'
	// Stateless terminator char
	int stc, // negative value disables the feature
	//
	// Tag generation.
	bool lkwd_ena, // Enable keyword lookup before tag push
	verilogKind gen_tag_kind // generate tags of this kind
){
	vStringClear(name);
	//
	int c;
	int blvl = 0, cblvl = 0, sqblvl = 0;
	bool eq_seen = false;
	bool stc_triggered;
	do{
		c = vGetc();
		//
		if(c == '(') blvl += 1;
		if(c == ')') blvl -= 1;
		//
		if(c == '{') cblvl += 1;
		if(c == '}') cblvl -= 1;
		//
		if(c == '[') sqblvl += 1;
		if(c == ']') sqblvl -= 1;
		//
		// Check for brace level underrun. Either it triggers an exit
		// condition, or it is a typo mistake to be corrected.
		if(blvl < 0){
			if(sens_b) {
				break;
			} else {
				blvl = 0;
			}
		}
		if(cblvl < 0){
			if(sens_cb) {
				break;
			} else {
				cblvl = 0;
			}
		}
		if(sqblvl < 0){
			if(sens_sqb) {
				break;
			} else {
				sqblvl = 0;
			}
		}
		// as negative values filtered out, this is a valid check for
		// being inside a level of braces
		bool inside_inner_braces = blvl || cblvl || sqblvl ;

		bool sepchar_detected =
			c == ',' && sep_comma ||
			c == ';' && sep_semicolon;

		if(!inside_inner_braces) {
			if(c == '=') {
				eq_seen = true;
			} else if (sepchar_detected) {
				eq_seen = false;
			}
		}

		stc_triggered = (stc < 0) ? false : (stc == c);

		if(inside_inner_braces || isspace(c) || sepchar_detected || eq_seen || stc_triggered) {
			// Separator event trigger: pushing what has been
			// accumulated, if any
			if(name -> length > 0) {
				if(isAGoodIdentifier(name)){
					if(!lkwd_ena || !checkIsKnownKeyword(name)){
						makeSimpleTag(name, gen_tag_kind);
					}
				}
				vStringClear(name);
			}
		} else {
			// Collecting characters
			vStringPut (name, c);
		}
	} while ( !stc_triggered );
}



/**
 * Detects enum-s to extract symbols.
 */
static void tagEnum(vString *const name, bool is_typedefed){
	int c;

	while(1){
		c = vGetc();
		if(c == ';') {
			// end of enum detected, some syntax mismatch
			return;
		} else if(c == '{') {
			// found state list start!
			break;
		}
		// skipping through symbols
	}

	// investigating the {} pair
	tagCommonAlgoNG (
		name,
		//
		false,     // ()
		true,      // {}
		false,     // []
		//
		true,      // ','
		false,     // ';'
		//
		-1,        // stc
		//
		false,     // lkwd_ena
		K_CONSTANT // gen_tag_kind
	);

	// Detecting enum identifiers (both typedef and variable)
	tagCommonAlgoNG (
		name,
		//
		false,     // ()
		false,     // {}
		false,     // []
		//
		true,      // ','
		false,     // ';'
		//
		';',       // stc
		//
		false,     // lkwd_ena
		(is_typedefed ? K_ENUM : K_LOGIC) // gen_tag_kind
	);
}



static void tagStructAndUnion(const verilogKind kind, vString *const name){
	int c;
	// Skip through union beginning, where some additional declarations may occur
	while(1){
		c = vGetc();
		if(isIdentifierCharacter(c) || isspace(c)){
			continue; // a mix of some inentifiers ans spaces, nothing other is allowed by standard
		}
		if(c == '{'){
			// finally the {} list!
			break;
		}
		return; // Garbage detected
	}
	// Detecting field list
	tagCommonAlgoNG (
		name,
		//
		false,     // ()
		true,      // {}
		false,     // []
		//
		true,      // ','
		true,      // ';'
		//
		-1,        // stc
		//
		true,      // lkwd_ena
		K_CONSTANT // gen_tag_kind
	);

	// Detecting trailing name declarations
	tagCommonAlgoNG (
		name,
		//
		false,     // ()
		false,     // {}
		false,     // []
		//
		true,      // ','
		false,     // ';'
		//
		';',       // stc
		//
		false,     // lkwd_ena
		kind       // gen_tag_kind
	);
}



static void tagTypedef(vString *const name){
	int c;
	// As for now a small sublayer for entering enum detection.
	c = vGetc();
	c = skipWhite(c);
	if(!readIdentifier(name, c)){
		return;
	}
	verilogKind kind = lookupParserKeyword(name);
	if(kind == K_ENUM){
		tagEnum(name, true);
	} else if(kind == K_STRUCT || kind == K_UNION){
		tagStructAndUnion(kind, name);
	}
}





/**
 * A parser for ANSI module parameter list in a
 * #(
 *    parameter some_type1   IDENTIFIER1 = c_expr1,
 *    parameter [some_type2] IDENTIFIER2 = c_expr2,
 *    ...
 * )
 * style
 * See [1], p.661.
 */
static void tagModuleParameterListANSI(vString *const name){
	tagCommonAlgoNG (
		name,
		//
		true,      // ()
		false,     // {}
		false,     // []
		//
		true,      // ','
		false,     // ';'
		//
		-1,        // stc
		//
		true,      // lkwd_ena
		K_CONSTANT // gen_tag_kind
	);
}


/**
 * A parser for module ports list
 */
static void tagModuleSignals(vString *const name){
	tagCommonAlgoNG (
		name,
		//
		true,      // ()
		false,     // {}
		false,     // []
		//
		true,      // ','
		false,     // ';'
		//
		-1,        // stc
		//
		true,      // lkwd_ena
		K_LOGIC // gen_tag_kind
	);
}

/**
 * Trying to detect the following construct:
 * module some_identifier #(......)(.......)
 * Where #(...) - list will be parsed in separate, and (....) is to
 * be also parsed to produce variables.
 */
static void tagModule(vString *const name) {
	int c = vGetc();
	c = skipWhite(c);
	if(!readIdentifier(name, c)){
		return;
	}
	makeSimpleTag(name, K_MODULE); /* detected module identifier */
	c = vGetc();
	c = skipWhite(c);
	if(c == '#') {
		c = vGetc();
		c = skipWhite(c);
		if(c != '(') {
			/* Stop parsing */
			vUngetc (c);
			return;
		}else{
			tagModuleParameterListANSI(name);
			c = vGetc();
			c = skipWhite(c);
			if(c == '(') {
				tagModuleSignals(name);
				return; /* Success */
			} else {
				vUngetc(c);
				return; /* Some abrupt completion, doesn't matter... */
			}
		}
	}else if(c == '(') {
		/* No port list for this module */
		tagModuleSignals(name);
		return;
	} else {
		/* Something unexpected, stop parsing */
		vUngetc (c);
		return;
	}
}


/**
 * Tags a comma-separated name list of an abstract something verilogish.
 */
static void tagNameList (const verilogKind kind, vString *const name)
{
	tagCommonAlgoNG (
		name,
		//
		false,     // ()
		false,     // {}
		false,     // []
		//
		true,      // ','
		false,     // ';'
		//
		';',       // stc
		//
		true,      // lkwd_ena
		kind       // gen_tag_kind
	);
}


static void tagMacro(vString *const name){
	int c = vGetc();
	c = skipWhite(c);
	if(!readIdentifier(name, c)){
		return;
	}
	makeSimpleTag(name, K_MACRO);
	// TODO: extend parsing beyond name tag detection!
}

static void tagPackage(vString *const name){
	while(1){
		int c = vGetc();
		c = skipWhite(c);
		if(c == ';'){
			break; // abrupt completion
		}
		if(!readIdentifier(name, c)){
			break; // garbage
		}
		if(checkIsKnownKeyword(name)){
			continue; // skip through possible keywords
		}
		makeSimpleTag(name, K_PACKAGE);
		break;
	}
}

static void findTag (vString *const name)
{
	const verilogKind kind = lookupParserKeyword(name);
	if(kind == K_MODULE)
	{
		tagModule(name);
	}
	else if(kind == K_ENUM)
	{
		tagEnum(name, false);
	}
	else if(kind == K_TYPEDEF)
	{
		tagTypedef(name);
	}
	else if(kind == K_MACRO)
	{
		tagMacro(name);
	}
	else if(kind == K_PACKAGE)
	{
		tagPackage(name);
	}
	else if(kind == K_OTHER)
	{
		// Doing nothing with these kinds, as they are only declared to
		// be recognizable as keywords, so as not to make their way into
		// the tags list.
	}
	else if(kind != K_UNDEFINED) {
		// All the remaining normal tag types fall here. Applying a
		// generic comma-separated variable list detection algorithm.
		tagNameList (kind, name);
	} else {
		// Likely a `typedef`ed type, denoted by it's identifier.
		// Without having a list of all such declared types, we cal only
		// assume, that every such construct is a kind of variable
		// declaration.
		if(isAGoodIdentifier(name)) tagNameList(K_CONSTANT, name);
	}
}

static void findVerilogTags (void)
{
	vString *const name = vStringNew ();
	volatile bool newStatement = true;
	volatile int c = '\0';
	exception_t exception = (exception_t) setjmp (Exception);

	if (exception == ExceptionNone) while (c != EOF)
	{
		c = vGetc ();
		switch (c)
		{
			case ';':
			case '\n':
				newStatement = true;
				break;

			case ' ':
			case '\t':
				break;

			default:
				if (newStatement && readIdentifier (name, c))
					findTag (name);
				newStatement = false;
				break;
		}
	}
	vStringDelete (name);
}

extern parserDefinition* VerilogParser (void)
{
	static const char *const extensions [] = { "v", "sv", NULL };
	parserDefinition* def = parserNew ("Verilog");
	def->kindTable  = VerilogKinds;
	def->kindCount  = ARRAY_SIZE (VerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initialize;
	return def;
}
