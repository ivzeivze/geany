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
	// non-functional language constructs, placed here to detectable and thus filterable
	{ "void",      K_OTHER},
	{ "packed",    K_OTHER}
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

static void vUngetc (int c)
{
	Assert (Ungetc == '\0');
	Ungetc = c;
}

static int vGetc (void)
{
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
					bool tag_kind_check_passed = true;
					if(lkwd_ena){
						const verilogKind kind1 = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
						tag_kind_check_passed = kind1 == K_UNDEFINED;
					}
					if(tag_kind_check_passed) makeSimpleTag(name, gen_tag_kind);
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
	const verilogKind kind = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
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


static void findTag (vString *const name)
{
	const verilogKind kind = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
	if (kind == K_CONSTANT && vStringItem (name, 0) == '`')
	{
		/* Bug #961001: Verilog compiler directives are line-based. */
		int c = skipWhite (vGetc ());
		readIdentifier (name, c);
		makeSimpleTag (name, kind);
		/* Skip the rest of the line. */
		do {
			c = vGetc();
		} while (c != '\n');
		vUngetc (c);
	}
	else if(kind == K_MODULE)
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
	else if(kind == K_MACRO) {
		tagMacro(name);
	}
	else if (kind != K_UNDEFINED)
	{
		tagNameList (kind, name);
	} else {
		// TODO
		//
		// This could be a `typedef`ed type, denoted by it's identifier.
		// Detecting such constructs is impossible without detecting all language keywords,
		// as otherwise the lists would be full of language constructs and because of false-triggering.

		//
		// if(isAGoodIdentifier(name)) tagNameList(K_CONSTANT, name);
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
	static const char *const extensions [] = { "v", NULL };
	parserDefinition* def = parserNew ("Verilog");
	def->kindTable  = VerilogKinds;
	def->kindCount  = ARRAY_SIZE (VerilogKinds);
	def->extensions = extensions;
	def->parser     = findVerilogTags;
	def->initialize = initialize;
	return def;
}
