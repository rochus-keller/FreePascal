#ifndef __CD_TOKENTYPE__
#define __CD_TOKENTYPE__
// This file was automatically generated by EbnfStudio; don't modify it!


#include <QByteArray>

namespace Cd {
	enum TokenType {
		Tok_Invalid = 0,

		TT_Literals,

		TT_Keywords,
		Tok_A,
		Tok_ALIGN,
		Tok_APPID,
		Tok_APPNAME,
		Tok_APPTYPE,
		Tok_ASMMODE,
		Tok_ASSERTIONS,
		Tok_B,
		Tok_BOOLEVAL,
		Tok_C,
		Tok_CALLING,
		Tok_D,
		Tok_DEBUGINFO,
		Tok_DEFINE,
		Tok_DESCRIPTION,
		Tok_E,
		Tok_ELSE,
		Tok_ENDIF,
		Tok_ERROR,
		Tok_EXTENDEDSYNTAX,
		Tok_F,
		Tok_FATAL,
		Tok_G,
		Tok_GOTO,
		Tok_H,
		Tok_HINT,
		Tok_HINTS,
		Tok_I,
		Tok_IF,
		Tok_IFDEF,
		Tok_IFNDEF,
		Tok_IFOPT,
		Tok_INCLUDE,
		Tok_INCLUDEPATH,
		Tok_INFO,
		Tok_INLINE,
		Tok_IOCHECKS,
		Tok_L,
		Tok_LIBRARYPATH,
		Tok_LINK,
		Tok_LINKLIB,
		Tok_LOCALSYMBOLS,
		Tok_LONGSTRINGS,
		Tok_M,
		Tok_MACRO,
		Tok_MAXFPUREGISTER,
		Tok_MEMORY,
		Tok_MESSAGE,
		Tok_MMX,
		Tok_MODE,
		Tok_N,
		Tok_NOTE,
		Tok_NOTES,
		Tok_O,
		Tok_OBJECTPATH,
		Tok_OPENSTRINGS,
		Tok_OUTPUT,
		Tok_OVERFLOWCHECKS,
		Tok_P,
		Tok_PACKENUM,
		Tok_PACKRECORDS,
		Tok_Q,
		Tok_R,
		Tok_RANGECHECKS,
		Tok_REFERENCEINFO,
		Tok_S,
		Tok_SATURATION,
		Tok_SMARTLINK,
		Tok_STACKFRAMES,
		Tok_STATIC,
		Tok_STOP,
		Tok_T,
		Tok_TYPEDADDRESS,
		Tok_TYPEINFO,
		Tok_UNDEF,
		Tok_UNITPATH,
		Tok_V,
		Tok_VARSTRINGCHECKS,
		Tok_VERSION,
		Tok_W,
		Tok_WAIT,
		Tok_WARNING,
		Tok_WARNINGS,
		Tok_X,
		Tok_Y,

		TT_Specials,
		Tok_Eof,

		TT_MaxToken,

		TT_Max
	};

	const char* tokenTypeString( int ); // Pretty with punctuation chars
	const char* tokenTypeName( int ); // Just the names without punctuation chars
	bool tokenTypeIsLiteral( int );
	bool tokenTypeIsKeyword( int );
	bool tokenTypeIsSpecial( int );
	TokenType tokenTypeFromString( const QByteArray& str, int* pos = 0 );
}
#endif // __CD_TOKENTYPE__