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
	{
		vStringClear(name);
		//
		unsigned int blvl = 0, cblvl = 1, sqblvl = 0;
		bool eq_seen = false;
		while(1){
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
			if(cblvl == 0) break;
			//
			bool inside_inner_braces = blvl > 0 || cblvl > 1 || sqblvl > 0;
			
			if(!inside_inner_braces) {
				if(c == '=') {
					eq_seen = true;
				} else if (c == ',') {
					eq_seen = false;
				}
			}
			
			
			if(inside_inner_braces || isspace(c) || c == ',' || eq_seen) {
				// Separator event trigger: pushing what has been
				// accumulated, if any
				if(name -> length > 0) {
					{ // push tag
						if(isAGoodIdentifier(name)){
							makeSimpleTag(name, K_CONSTANT);
						}
					}
					vStringClear(name);
				}
			} else {
				// Collecting characters
				vStringPut (name, c);
			}
		}
	}
	
	// Detecting enum identifiers (both typedef and variable)
	{
		vStringClear(name);
		bool end_detected;
		do{
			c = vGetc();
			end_detected = (c == ';');
			//
			if(isspace(c) || c == ',' || end_detected){
				if(name -> length > 0) {
					if(isAGoodIdentifier(name)){
						makeSimpleTag(name, is_typedefed ? K_ENUM : K_LOGIC);
					}
					vStringClear(name);
				}
			}else{
				vStringPut (name, c);
			}
		}while(!end_detected);
	}
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
	{
		vStringClear(name);
		unsigned int blvl = 0, cblvl = 1, sqblvl = 0;
		bool eq_seen = false;
		int state = 0;
		while(1){
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
			
			if(cblvl == 0) break;
			
			//
			bool inside_inner_braces = blvl > 0 || cblvl > 1 || sqblvl > 0;
			if(!inside_inner_braces) {
				if(c == '=') {
					eq_seen = true;
				} else if (c == ',' || c == ';') {
					eq_seen = false;
				}
			}
			
			if(inside_inner_braces || isspace(c) || c == ',' || c == ';' || eq_seen) {
				// Separator event trigger: pushing what has been
				// accumulated, if any
				if(name -> length > 0) {
					{ // push tag
						if(isAGoodIdentifier(name)){
							const verilogKind kind1 = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
							if(kind1 == K_UNDEFINED){
								makeSimpleTag(name, K_CONSTANT);
							}
						}
					}
					vStringClear(name);
				}
			} else {
				// Collecting characters
				vStringPut (name, c);
			}
		}
	}
	// Detecting trailing name declarations
	{
		vStringClear(name);
		bool end_detected;
		do{
			c = vGetc();
			end_detected = (c == ';');
			//
			if(isspace(c) || c == ',' || end_detected){
				if(name -> length > 0) {
					if(isAGoodIdentifier(name)){
						makeSimpleTag(name, kind);
					}
					vStringClear(name);
				}
			}else{
				vStringPut (name, c);
			}
		}while(!end_detected);
	}
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




// Checks a sequence of something, that has been found inside #(...)
// parameter list, checking it to be a good candidate for being a tag
static void pushParamTagCandidate(vString *const name) {
	if(! isAGoodIdentifier(name)) {
		// Does not comply with Verilog identifier naming convention
		return;
	}
	const verilogKind kind = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
	if(kind != K_UNDEFINED){
		return; // This is some keyword
	}
	
	makeSimpleTag(name, K_CONSTANT);
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
static void tagModuleParameterListANSI(vString *const name, int c){
	Assert(c == '(');
	/* Bracket counters */
	unsigned int mlvl = 1; // () , this is also used to determine the stop condition
	unsigned int cblvl = 0, sqblvl = 0; // {}, [], just to skip the contents, if any
	bool eq_seen = false; // flag, set, when equal "=" is seen, and unset by comma ","
	vStringClear(name);
	while(1) {
		c = vGetc();
		{ /* Manipulating braces */
			if(c == '(') mlvl += 1;
			if(c == ')') mlvl -= 1;
			//
			if(c == '{') cblvl += 1;
			if(c == '}') cblvl -= 1;
			//
			if(c == '[') sqblvl += 1;
			if(c == ']') sqblvl -= 1;
			//
			if(mlvl == 0) break;
		}
		/* A stream of chars as within #(....) braces as entry events is observable here */
		bool inside_inner_braces = cblvl > 0 || sqblvl > 0 || mlvl > 1;
		
		if(!inside_inner_braces) {
			if(c == '=') {
				eq_seen = true;
			} else if (c == ',') {
				eq_seen = false;
			}
		}
		
		if(inside_inner_braces || isspace(c) || c == ',' || eq_seen) {
			// Separator event trigger: pushing what has been
			// accumulated, if any
			if(name -> length > 0) {
				pushParamTagCandidate(name);
				vStringClear(name);
			}
		} else {
			// Collecting characters
			vStringPut (name, c);
		}
	}
}


// A parser for (...) contents, where module signals are defined
static void tagModuleSignals(vString *const name, int c){
	Assert(c == '(');
	/* Bracket counters */
	unsigned int mlvl = 1; // () , this is also used to determine the stop condition
	unsigned int cblvl = 0, sqblvl = 0; // {}, [], just to skip the contents, if any
	bool eq_seen = false; // flag, set, when equal "=" is seen, and unset by comma ","
	vStringClear(name);
	while(1) {
		c = vGetc();
		{ /* Manipulating braces */
			if(c == '(') mlvl += 1;
			if(c == ')') mlvl -= 1;
			//
			if(c == '{') cblvl += 1;
			if(c == '}') cblvl -= 1;
			//
			if(c == '[') sqblvl += 1;
			if(c == ']') sqblvl -= 1;
			//
			if(mlvl == 0) break;
		}
		/* A stream of chars as within #(....) braces as entry events is observable here */
		bool inside_inner_braces = cblvl > 0 || sqblvl > 0 || mlvl > 1;
		
		if(!inside_inner_braces) {
			if(c == '=') {
				eq_seen = true;
			} else if (c == ',') {
				eq_seen = false;
			}
		}
		
		if(inside_inner_braces || isspace(c) || c == ',' || eq_seen) {
			// Separator event trigger: pushing what has been
			// accumulated, if any
			if(name -> length > 0) {
				{ // push tag
					if(isAGoodIdentifier(name)){
						const verilogKind kind1 = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
						if(kind1 == K_UNDEFINED){
							makeSimpleTag(name, K_LOGIC);
						}
					}
				}
				vStringClear(name);
			}
		} else {
			// Collecting characters
			vStringPut (name, c);
		}
	}
}



static void tagModule(vString *const name) {
	
	/** 
	 * Trying to detect the following construct:
	 * module some_identifier #(......)(.......)
	 * Where #(...) - list will be parsed in separate, and (....) is to
	 * be also parsed to produce variables.
	 */
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
			tagModuleParameterListANSI(name, c);
			c = vGetc();
			c = skipWhite(c);
			if(c == '(') {
				tagModuleSignals(name, c);
				vUngetc(c);
				return; /* Success */
			} else {
				vUngetc(c);
				return; /* Some abrupt completion, doesn't matter... */
			}
		}
	}else if(c == '(') {
		/* Skip beyond the port list */
		tagModuleSignals(name, c);
		vUngetc (c);
		return;
	} else {
		/* Something unexpected, stop parsing */
		vUngetc (c);
		return;
	}
}


// Tags a comma-separated name list of an abstract something verilogish.
// @param name - the first actual tag in the list detected by previous stage.
// @param king - initial tag kind, that triggered the dissection
static void tagNameList (const verilogKind kind, vString *const name)
{
	vStringClear(name);
	int c;
	//
	unsigned int blvl = 0, cblvl = 0, sqblvl = 0;
	bool eq_seen = false;
	bool stop_condition;
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
		bool inside_inner_braces = blvl > 0 || cblvl > 0 || sqblvl > 0;
		
		if(!inside_inner_braces) {
			if(c == '=') {
				eq_seen = true;
			} else if (c == ',') {
				eq_seen = false;
			}
		}
		
		stop_condition = (c == ';');  // semicolon is used as quite reliable condition
		
		if(inside_inner_braces || isspace(c) || c == ',' || eq_seen || stop_condition ) {
			if(name -> length > 0) {
				// push tag
				const verilogKind kind1 = (verilogKind) lookupKeyword (vStringValue (name), Lang_verilog);
				if(kind1 == K_UNDEFINED && isAGoodIdentifier(name)){
					// A check, that this is not a stray identifier somehow + naming conformance
					makeSimpleTag(name, kind);
				}
				vStringClear(name);
			}
		} else {
			// Collecting characters
			vStringPut (name, c);
		}
		
	}while(!stop_condition);
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
