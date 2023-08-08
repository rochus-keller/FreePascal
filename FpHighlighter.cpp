/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the FreePascal Code Navigator application.
*
* The following is the license that applies to this copy of the
* application. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

// Adopted from https://github.com/rochus-keller/LisaPascal/

#include "FpHighlighter.h"
#include "FpLexer.h"
#include <QBuffer>
using namespace Fp;

PascalPainter::PascalPainter(QObject* parent) :
    QSyntaxHighlighter(parent)
{
    for( int i = 0; i < C_Max; i++ )
    {
        d_format[i].setFontWeight(QFont::Normal);
        d_format[i].setForeground(Qt::black);
        d_format[i].setBackground(Qt::transparent);
    }
    d_format[C_Num].setForeground(QColor(0, 153, 153));
    d_format[C_Str].setForeground(QColor(208, 16, 64));
    d_format[C_Cmt].setForeground(QColor(153, 153, 136));
    d_format[C_Kw].setForeground(QColor(68, 85, 136));
    d_format[C_Kw].setFontWeight(QFont::Bold);
    d_format[C_Op].setForeground(QColor(153, 0, 0));
    d_format[C_Op].setFontWeight(QFont::Bold);
    d_format[C_Type].setForeground(QColor(153, 0, 115));
    d_format[C_Type].setFontWeight(QFont::Bold);
    d_format[C_Pp].setFontWeight(QFont::Bold);
    d_format[C_Pp].setForeground(QColor(0, 128, 0));
    d_format[C_Pp].setBackground(QColor(230, 255, 230));
    d_format[C_Label].setForeground(QColor(251, 138, 0));
    d_format[C_Label].setBackground(QColor(253, 217, 165));
}

void PascalPainter::addBuiltIn(const QByteArray& bi)
{
    d_builtins << bi.toUpper();
}

void PascalPainter::addKeyword(const QByteArray& kw)
{
    d_keywords << kw;
}

void PascalPainter::addBuiltIns()
{
    const char** str = s_builtInTypes;
    while(*str)
    {
        d_builtins << *str;
        str++;
    }
    str = s_builtInConsts;
    while(*str)
    {
        d_builtins << *str;
        str++;
    }
    str = s_builtInProcs;
    while(*str)
    {
        d_builtins << *str;
        str++;
    }

}

QTextCharFormat PascalPainter::formatForCategory(int c) const
{
    return d_format[c];
}

void PascalPainter::highlightBlock(const QString& text)
{
    const int previousBlockState_ = previousBlockState();
    int lexerState = 0, initialBraceDepth = 0;
    if (previousBlockState_ != -1) {
        lexerState = previousBlockState_ & 0xff;
        initialBraceDepth = previousBlockState_ >> 8;
    }

    int braceDepth = initialBraceDepth;

    // Protocol:
    // lexerState == 1: multi-line (* *) comment
    // lexerState == 2: multi-line { } comment
    // lexerState == 4: {$ } directive

    int start = 0;
    if( lexerState == 1 || lexerState == 3 )
    {
        QTextCharFormat f = formatForCategory( lexerState == 1 ? C_Cmt : C_Pp);
        int pos = text.indexOf("*)");
        if( pos == -1 )
        {
            // the whole block ist part of comment
            setFormat( start, text.size(), f );
            setCurrentBlockState( (braceDepth << 8) | lexerState);
            return;
        }else
        {
            // End of comment found
            pos += 2;
            setFormat( start, pos , f );
            lexerState = 0;
            braceDepth--;
            start = pos;
        }
    }else if( lexerState == 2 || lexerState == 4 )
    {
        QTextCharFormat f = formatForCategory( lexerState == 2 ? C_Cmt : C_Pp);
        int pos = text.indexOf('}');
        if( pos == -1 )
        {
            // the whole block ist part of comment
            setFormat( start, text.size(), f );
            setCurrentBlockState( (braceDepth << 8) | lexerState);
            return;
        }else
        {
            // End of comment found
            pos += 1;
            setFormat( start, pos , f );
            lexerState = 0;
            braceDepth--;
            start = pos;
        }
    }


    Lexer lex;
    lex.setIgnoreComments(false);
    lex.setPackComments(false);

    QList<Token> tokens = lex.tokens(text.mid(start));
    for( int i = 0; i < tokens.size(); ++i )
    {
        Token &t = tokens[i];
        t.d_colNr += start;

        QTextCharFormat f;
        if( t.d_type == Tok_Latt )
        {
            f = formatForCategory(C_Cmt);
            lexerState = 1;
            if( t.d_val.endsWith("*)") )
                lexerState = 0;
            else
                braceDepth++;
        }else if( t.d_type == Tok_Lbrace )
        {
            if( t.d_val.startsWith("{$") )
            {
                f = formatForCategory(C_Pp);
                lexerState = 4;
            }else
            {
                f = formatForCategory(C_Cmt);
                lexerState = 2;
            }
            if( t.d_val.endsWith("}") )
                lexerState = 0;
            else
                braceDepth++;
        }else if( t.d_type == Tok_quoted_string )
            f = formatForCategory(C_Str);
        else if( t.d_type == Tok_unsigned_real || t.d_type == Tok_decimal_int || t.d_type == Tok_hex_int
                 || t.d_type == Tok_octal_int || t.d_type == Tok_binary_int )
            f = formatForCategory(C_Num);
        else if( tokenTypeIsLiteral(t.d_type) )
        {
            f = formatForCategory(C_Op);
        }else if( tokenTypeIsKeyword(t.d_type) || tokenTypeIsKeyword(t.d_code) )
        {
            f = formatForCategory(C_Kw);
        }else if( t.d_type == Tok_ident )
        {
            const QByteArray& name = t.d_val.toUpper();
            /*if( i+1 < tokens.size() && tokens[i+1].d_type == Tok_Colon)
                f = formatForCategory(C_Label);
            else */if( d_builtins.contains(name) )
                f = formatForCategory(C_Type);
            else if( d_keywords.contains(name) )
                f = formatForCategory(C_Kw);
            else
                f = formatForCategory(C_Ident);
        }

        /*if( lexerState == 3 )
            setFormat( startPp, t.d_colNr - startPp + t.d_val.size(), formatForCategory(C_Pp) );
        else */
        if( f.isValid() )
            setFormat( t.d_colNr-1, t.d_val.isEmpty() ? t.d_len : t.d_val.size(), f );
    }

    setCurrentBlockState((braceDepth << 8) | lexerState );
}

LogPainter::LogPainter(QTextDocument* parent):QSyntaxHighlighter(parent)
{

}

void LogPainter::highlightBlock(const QString& text)
{
    QColor c = Qt::black;
    if( text.startsWith("WRN:") )
        c = Qt::blue;
    else if( text.startsWith("ERR:") )
        c = Qt::red;

    setFormat( 0, text.size(), c );
}

// according to https://www.freepascal.org/docs-html/rtl/system/
const char* PascalPainter::s_builtInTypes[] = {
    "ANSICHAR",
    "ANSISTRING",
    "BYTE",
    "BOOLEAN",
    "CARDINAL",
    "CHAR",
    "CODEPOINTER",
    "CODEPOINTER",
    "CODEPTRINT",
    "CODEPTRINT",
    "CODEPTRUINT",
    "CODEPTRUINT",
    "COMP",
    "DWORD",
    "ENUMRESLANGPROC",
    "ENUMRESNAMEPROC",
    "ENUMRESTYPEPROC",
    "FARPOINTER",
    "FILEREC",
    "HGLOBAL",
    "HMODULE",
    "HRESULT",
    "IINTERFACE",
    "INT16",
    "INT32",
    "INT64",
    "INT8",
    "INTEGER",
    "INTEGERARRAY",
    "INTPTR",
    "JMP_BUF",
    "LONGINT",
    "LONGWORD",
    "MAKEINTRESOURCE",
    "MARSHALEDASTRING",
    "MARSHALEDSTRING",
    "NATIVEINT",
    "NATIVEUINT",
    "OPAQUEPOINTER",
    "PANSICHAR",
    "PANSISTRING",
    "PBOOLEAN",
    "PBOOLEAN16",
    "PBOOLEAN32",
    "PBOOLEAN64",
    "PBOOLEAN8",
    "PBYTE",
    "PBYTEBOOL",
    "PCALLDESC",
    "PCARDINAL",
    "PCHAR",
    "PCLASS",
    "PCODEPOINTER",
    "PCURRENCY",
    "PDATE",
    "PDATETIME",
    "PDISPATCH",
    "PDISPDESC",
    "PDOUBLE",
    "PDWORD",
    "PDYNARRAYINDEX",
    "PDYNARRAYTYPEINFO",
    "PERROR",
    "PEVENTSTATE",
    "PEXCEPTADDR",
    "PEXCEPTOBJECT",
    "PEXTENDED",
    "PFILETEXTRECCHAR",
    "PGUID",
    "PINT16",
    "PINT32",
    "PINT64",
    "PINT8",
    "PINTEGER",
    "PINTEGERARRAY",
    "PINTERFACE",
    "PINTERFACEENTRY",
    "PINTERFACETABLE",
    "PINTPTR",
    "PJMP_BUF",
    "PLONGBOOL",
    "PLONGINT",
    "PLONGWORD",
    "PMARSHALEDASTRING",
    "PMARSHALEDSTRING",
    "PMEMORYMANAGER",
    "PMETHOD",
    "PMSGSTRTABLE",
    "PNATIVEINT",
    "PNATIVEUINT",
    "POINTERARRAY",
    "POLEVARIANT",
    "POPAQUEDATA",
    "PPANSICHAR",
    "PPBYTE",
    "PPCHAR",
    "PPCHARARRAY",
    "PPCODEPOINTER",
    "PPDISPATCH",
    "PPDOUBLE",
    "PPDYNARRAYTYPEINFO",
    "PPLONGINT",
    "PPOINTER",
    "PPOINTERARRAY",
    "PPPANSICHAR",
    "PPPCHAR",
    "PPPOINTER",
    "PPPWIDECHAR",
    "PPTRINT",
    "PPTRUINT",
    "PPUNKNOWN",
    "PPVMT",
    "PPWIDECHAR",
    "PQWORD",
    "PQWORDBOOL",
    "PRAWBYTESTRING",
    "PRTLCRITICALSECTION",
    "PRTLEVENT",
    "PSHORTINT",
    "PSHORTSTRING",
    "PSINGLE",
    "PSIZEINT",
    "PSIZEUINT",
    "PSMALLINT",
    "PSTRINGMESSAGETABLE",
    "PTEXT",
    "PTRINT",
    "PTRINT",
    "PTRUINT",
    "PTRUINT",
    "PUCS2CHAR",
    "PUCS4CHAR",
    "PUCS4CHARARRAY",
    "PUINT16",
    "PUINT32",
    "PUINT64",
    "PUINT8",
    "PUINTPTR",
    "PUNICODECHAR",
    "PUNICODESTRING",
    "PUNKNOWN",
    "PUTF8CHAR",
    "PUTF8STRING",
    "PVARARRAY",
    "PVARARRAYBOUND",
    "PVARARRAYBOUNDARRAY",
    "PVARARRAYCOORARRAY",
    "PVARDATA",
    "PVARIANT",
    "PVARIANTMANAGER",
    "PVARREC",
    "PVMT",
    "PWIDECHAR",
    "PWIDESTRING",
    "PWORD",
    "PWORDBOOL",
    "QWORD",
    "RAWBYTESTRING",
    "REAL",
    "REAL48",
    "SHORTINT",
    "SIZEINT",
    "SIZEINT",
    "SIZEUINT",
    "SIZEUINT",
    "SMALLINT",
    "TABSTRACTERRORPROC",
    "TALLOCATETHREADVARSHANDLER",
    "TANSICHAR",
    "TASSERTERRORPROC",
    "TBACKTRACESTRFUNC",
    "TBASICEVENTCREATEHANDLER",
    "TBASICEVENTHANDLER",
    "TBASICEVENTWAITFORHANDLER",
    "TBEGINTHREADHANDLER",
    "TBOUNDARRAY",
    "TCALLDESC",
    "TCLASS",
    "TCOMPAREOPTION",
    "TCOMPAREOPTIONS",
    "TCRITICALSECTIONHANDLER",
    "TCRITICALSECTIONHANDLERTRYENTER",
    "TCTRLBREAKHANDLER",
    "TDATE",
    "TDATETIME",
    "TDISPDESC",
    "TDOUBLEREC",
    "TDYNARRAYINDEX",
    "TDYNARRAYTYPEINFO",
    "TDYNLIBSMANAGER",
    "TENDTHREADHANDLER",
    "TENTRYINFORMATION",
    "TENTRYINFORMATIONOS",
    "TERROR",
    "TERRORPROC",
    "TEXCEPTADDR",
    "TEXCEPTOBJECT",
    "TEXCEPTPROC",
    "TEXTBUF",
    "TEXTENDED80REC",
    "TEXTFILE",
    "TEXTREC",
    "TFILETEXTRECCHAR",
    "TFLOATSPECIAL",
    "TFPCHEAPSTATUS",
    "TFPRESOURCEHANDLE",
    "TFPRESOURCEHGLOBAL",
    "TFPRESOURCEHMODULE",
    "TFPUEXCEPTION",
    "TFPUEXCEPTIONMASK",
    "TFPUPRECISIONMODE",
    "TFPUROUNDINGMODE",
    "TGETCURRENTTHREADIDHANDLER",
    "TGETLOADERRORSTRHANDLER",
    "TGETPROCADDRESSHANDLER",
    "TGETPROCADDRESSORDINALHANDLER",
    "TGUID",
    "THANDLE",
    "THEAPSTATUS",
    "TINITTHREADVARHANDLER",
    "TINTERFACEDCLASS",
    "TINTERFACEENTRY",
    "TINTERFACEENTRYTYPE",
    "TINTERFACETABLE",
    "TLIBHANDLE",
    "TLINEENDSTR",
    "TLOADLIBRARYAHANDLER",
    "TLOADLIBRARYUHANDLER",
    "TMEMORYMANAGER",
    "TMETHOD",
    "TMSGSTRTABLE",
    "TOPAQUEDATA",
    "TORDINALENTRY",
    "TPCHARARRAY",
    "TPROCEDURE",
    "TRELEASETHREADVARSHANDLER",
    "TRELOCATETHREADVARHANDLER",
    "TRESOURCEHANDLE",
    "TRESOURCEMANAGER",
    "TRTLCREATEEVENTHANDLER",
    "TRTLCRITICALSECTION",
    "TRTLEVENTHANDLER",
    "TRTLEVENTHANDLERTIMEOUT",
    "TRTLMETHOD",
    "TRUNTIMEERROR",
    "TSAFECALLERRORPROC",
    "TSEMAPHOREDESTROYHANDLER",
    "TSEMAPHOREPOSTHANDLER",
    "TSEMAPHOREWAITHANDLER",
    "TSEMPAHOREINITHANDLER",
    "TSINGLEREC",
    "TSTANDARDCODEPAGEENUM",
    "TSTRINGMESSAGETABLE",
    "TSYSTEMCODEPAGE",
    "TTEXTBUF",
    "TTEXTLINEBREAKSTYLE",
    "TTHREADFUNC",
    "TTHREADGETPRIORITYHANDLER",
    "TTHREADHANDLER",
    "TTHREADID",
    "TTHREADMANAGER",
    "TTHREADSETPRIORITYHANDLER",
    "TTHREADSETTHREADDEBUGNAMEHANDLERA",
    "TTHREADSETTHREADDEBUGNAMEHANDLERU",
    "TTHREADSWITCHHANDLER",
    "TTIME",
    "TTYPEKIND",
    "TUCS4CHARARRAY",
    "TUNICODESTRINGMANAGER",
    "TUNLOADLIBRARYHANDLER",
    "TVARARRAY",
    "TVARARRAYBOUND",
    "TVARARRAYBOUNDARRAY",
    "TVARARRAYCOORARRAY",
    "TVARDATA",
    "TVARIANTMANAGER",
    "TVAROP",
    "TVARREC",
    "TVARTYPE",
    "TVMT",
    "TWAITFORTHREADTERMINATEHANDLER",
    "TWIDESTRINGMANAGER",
    "UCS2CHAR",
    "UCS4CHAR",
    "UCS4STRING",
    "UINT16",
    "UINT32",
    "UINT64",
    "UINT8",
    "UINTPTR",
    "UNICODECHAR",
    "UNICODESTRING",
    "UTF8CHAR",
    "UTF8STRING",
    "VALREAL",
    "VALSINT",
    "VALSINT",
    "VALUINT",
    "VALUINT",
    "WCHAR",
    "WIDECHAR",
    "WIDESTRING",
    "WORD",
    "IDISPATCH",
    "IENUMERABLE",
    "IENUMERATOR",
    "IINVOKABLE",
    "IUNKNOWN",
    "TAGGREGATEDOBJECT",
    "TCONTAINEDOBJECT",
    "TINTERFACEDOBJECT",
    "TOBJECT",
    0
};

const char* PascalPainter::s_builtInConsts[] = {
    "TRUE",
    "FALSE",
    "ABSTRACTERRORPROC",
    "ALLFILESMASK",
    "ALLOWDIRECTORYSEPARATORS",
    "ALLOWDRIVESEPARATORS",
    "ASSERTERRORPROC",
    "BACKTRACESTRFUNC",
    "CATCHALLEXCEPTIONS",
    "CEXCEPTIONFRAME",
    "CFINALIZEFRAME",
    "CP_ACP",
    "CP_ASCII",
    "CP_NONE",
    "CP_OEMCP",
    "CP_UTF16",
    "CP_UTF16BE",
    "CP_UTF7",
    "CP_UTF8",
    "CTRLZMARKSEOF",
    "DEFAULT8087CW",
    "DEFAULTMXCSR",
    "DEFAULTSTACKSIZE",
    "DEFAULTTEXTLINEBREAKSTYLE",
    "DIRECTORYSEPARATOR",
    "DRIVESEPARATOR",
    "ERRORADDR",
    "ERRORCODE",
    "ERRORPROC",
    "EXCEPTCLSPROC",
    "EXCEPTOBJPROC",
    "EXCEPTPROC",
    "EXITPROC",
    "EXTENSIONSEPARATOR",
    "E_NOINTERFACE",
    "E_NOTIMPL",
    "E_UNEXPECTED",
    "FILEMODE",
    "FILENAMECASEPRESERVING",
    "FILENAMECASESENSITIVE",
    "FILERECNAMELENGTH",
    "FLOAT_FLAG_DENORMAL",
    "FLOAT_FLAG_DIVBYZERO",
    "FLOAT_FLAG_INEXACT",
    "FLOAT_FLAG_INVALID",
    "FLOAT_FLAG_OVERFLOW",
    "FLOAT_FLAG_UNDERFLOW",
    "FLOAT_ROUND_DOWN",
    "FLOAT_ROUND_NEAREST_EVEN",
    "FLOAT_ROUND_TO_ZERO",
    "FLOAT_ROUND_UP",
    "FMAPPEND",
    "FMCLOSED",
    "FMINOUT",
    "FMINPUT",
    "FMOUTPUT",
    "FPC_EXCEPTION",
    "GROWHEAPSIZE1",
    "GROWHEAPSIZE2",
    "GROWHEAPSIZESMALL",
    "HAS_MMX_SUPPORT",
    "HAS_SSE2_SUPPORT",
    "HAS_SSE3_SUPPORT",
    "HAS_SSE_SUPPORT",
    "INITPROC",
    "IOBJECTINSTANCE",
    "ISMULTITHREAD",
    "LFNSUPPORT",
    "LINEENDING",
    "MAXEXITCODE",
    "MAXINT",
    "MAXKEPTOSCHUNKS",
    "MAXLONGINT",
    "MAXPATHLEN",
    "MAXSINTVALUE",
    "MAXSMALLINT",
    "MAXUINTVALUE",
    "MAX_FRAME_DUMP",
    "MODULEISCPP",
    "MODULEISLIB",
    "MODULEISPACKAGE",
    "NILHANDLE",
    "PATHSEPARATOR",
    "RAISEMAXFRAMECOUNT",
    "RAISEPROC",
    "RT_ACCELERATOR",
    "RT_ANICURSOR",
    "RT_ANIICON",
    "RT_BITMAP",
    "RT_CURSOR",
    "RT_DIALOG",
    "RT_FONT",
    "RT_FONTDIR",
    "RT_GROUP_CURSOR",
    "RT_GROUP_ICON",
    "RT_HTML",
    "RT_ICON",
    "RT_MANIFEST",
    "RT_MENU",
    "RT_MESSAGETABLE",
    "RT_RCDATA",
    "RT_STRING",
    "RT_VERSION",
    "RUNTIMEERROREXITCODES",
    "SAFECALLERRORPROC",
    "SHAREDSUFFIX",
    "SLINEBREAK",
    "STACKERROR",
    "STDERRORHANDLE",
    "STDINPUTHANDLE",
    "STDOUTPUTHANDLE",
    "S_FALSE",
    "S_OK",
    "TEST8086",
    "TEST8087",
    "TEXTRECBUFSIZE",
    "TEXTRECNAMELENGTH",
    "THREADINGALREADYUSED",
    "TKANSICHAR",
    "TKANSISTRING",
    "TKSHORTSTRING",
    "TKUNICODESTRING",
    "TKWIDECHAR",
    "TKWIDESTRING",
    "UNIXGETMODULEBYADDRHOOK",
    "UNUSEDHANDLE",
    "VARADDREFPROC",
    "VARANY",
    "VARARRAY",
    "VARBOOLEAN",
    "VARBYREF",
    "VARBYTE",
    "VARCLEARPROC",
    "VARCOPYPROC",
    "VARCURRENCY",
    "VARDATE",
    "VARDECIMAL",
    "VARDISPATCH",
    "VARDOUBLE",
    "VAREMPTY",
    "VARERROR",
    "VARINT64",
    "VARINTEGER",
    "VARLONGWORD",
    "VARNULL",
    "VAROLESTR",
    "VARQWORD",
    "VARRECORD",
    "VARSHORTINT",
    "VARSINGLE",
    "VARSMALLINT",
    "VARSTRARG",
    "VARSTRING",
    "VARTOLSTRPROC",
    "VARTOWSTRPROC",
    "VARTYPEMASK",
    "VARUINT64",
    "VARUNKNOWN",
    "VARUSTRARG",
    "VARUSTRING",
    "VARVARIANT",
    "VARWORD",
    "VARWORD64",
    "VMTAFTERCONSTRUCTION",
    "VMTAUTOTABLE",
    "VMTBEFOREDESTRUCTION",
    "VMTCLASSNAME",
    "VMTDEFAULTHANDLER",
    "VMTDEFAULTHANDLERSTR",
    "VMTDESTROY",
    "VMTDISPATCH",
    "VMTDISPATCHSTR",
    "VMTDYNAMICTABLE",
    "VMTEQUALS",
    "VMTFIELDTABLE",
    "VMTFREEINSTANCE",
    "VMTGETHASHCODE",
    "VMTINITTABLE",
    "VMTINSTANCESIZE",
    "VMTINTFTABLE",
    "VMTMETHODSTART",
    "VMTMETHODTABLE",
    "VMTMSGSTRPTR",
    "VMTNEWINSTANCE",
    "VMTPARENT",
    "VMTSAFECALLEXCEPTION",
    "VMTTOSTRING",
    "VMTTYPEINFO",
    "VTANSISTRING",
    "VTBOOLEAN",
    "VTCHAR",
    "VTCLASS",
    "VTCURRENCY",
    "VTEXTENDED",
    "VTINT64",
    "VTINTEGER",
    "VTINTERFACE",
    "VTOBJECT",
    "VTPCHAR",
    "VTPOINTER",
    "VTPWIDECHAR",
    "VTQWORD",
    "VTSTRING",
    "VTUNICODESTRING",
    "VTVARIANT",
    "VTWIDECHAR",
    "VTWIDESTRING",
    0
};

const char* PascalPainter::s_builtInProcs[] = {
    "ABS",
    "ABSTRACTERROR",
    "ACQUIREEXCEPTIONOBJECT",
    "ADD(VARIANT,VARIANT):VARIANT",
    "ADDEXITPROC",
    "ADDR",
    "ALIGN",
    "ALLOCMEM",
    "ANSITOUTF8",
    "APPEND",
    "ARCTAN",
    "ARRAYSTRINGTOPPCHAR",
    "ASSERT",
    "ASSIGN",
    "ASSIGN(COMP):OLEVARIANT",
    "ASSIGN(COMP):VARIANT",
    "ASSIGN(EXTENDED):OLEVARIANT",
    "ASSIGN(EXTENDED):VARIANT",
    "ASSIGN(OLEVARIANT):COMP",
    "ASSIGN(OLEVARIANT):EXTENDED",
    "ASSIGN(OLEVARIANT):REAL",
    "ASSIGN(OLEVARIANT):SINGLE",
    "ASSIGN(OLEVARIANT):UNICODESTRING",
    "ASSIGN(REAL):OLEVARIANT",
    "ASSIGN(REAL):VARIANT",
    "ASSIGN(REAL48):EXTENDED",
    "ASSIGN(SINGLE):OLEVARIANT",
    "ASSIGN(SINGLE):VARIANT",
    "ASSIGN(UCS4STRING):VARIANT",
    "ASSIGN(UNICODESTRING):OLEVARIANT",
    "ASSIGN(UNICODESTRING):VARIANT",
    "ASSIGN(UTF8STRING):VARIANT",
    "ASSIGN(VARIANT):COMP",
    "ASSIGN(VARIANT):EXTENDED",
    "ASSIGN(VARIANT):REAL",
    "ASSIGN(VARIANT):SINGLE",
    "ASSIGN(VARIANT):UNICODESTRING",
    "ASSIGN(VARIANT):UTF8STRING",
    "ASSIGNED",
    "BASICEVENTCREATE",
    "BASICEVENTDESTROY",
    "BASICEVENTRESETEVENT",
    "BASICEVENTSETEVENT",
    "BASICEVENTWAITFOR",
    "BEGINTHREAD",
    "BETON",
    "BINSTR",
    "BLOCKREAD",
    "BLOCKWRITE",
    "BREAK",
    "BSFBYTE",
    "BSFDWORD",
    "BSFQWORD",
    "BSFWORD",
    "BSRBYTE",
    "BSRDWORD",
    "BSRQWORD",
    "BSRWORD",
    "CAPTUREBACKTRACE",
    "CHDIR",
    "CHR",
    "CLOSE",
    "CLOSETHREAD",
    "COMPAREBYTE",
    "COMPARECHAR",
    "COMPARECHAR0",
    "COMPAREDWORD",
    "COMPAREWORD",
    "CONCAT",
    "CONTINUE",
    "COPY",
    "COPYARRAY",
    "COS",
    "CSEG",
    "DEC",
    "DEFAULT",
    "DEFAULTANSI2UNICODEMOVE",
    "DEFAULTANSI2WIDEMOVE",
    "DEFAULTUNICODE2ANSIMOVE",
    "DELETE",
    "DISPOSE",
    "DIVIDE(VARIANT,VARIANT):VARIANT",
    "DONECRITICALSECTION",
    "DONETHREAD",
    "DSEG",
    "DUMPEXCEPTIONBACKTRACE",
    "DUMP_STACK",
    "DYNARRAYBOUNDS",
    "DYNARRAYCLEAR",
    "DYNARRAYDIM",
    "DYNARRAYINDEX",
    "DYNARRAYSETLENGTH",
    "DYNARRAYSIZE",
    "EMPTYMETHOD",
    "ENDTHREAD",
    "ENTERCRITICALSECTION",
    "ENUMRESOURCELANGUAGES",
    "ENUMRESOURCENAMES",
    "ENUMRESOURCETYPES",
    "EOF",
    "EOLN",
    "EQUAL(VARIANT,VARIANT):BOOLEAN",
    "ERASE",
    "ERROR",
    "EXCLUDE",
    "EXIT",
    "EXP",
    "FAIL",
    "FILEPOS",
    "FILESIZE",
    "FILLBYTE",
    "FILLCHAR",
    "FILLDWORD",
    "FILLWORD",
    "FINALIZE",
    "FINALIZEARRAY",
    "FINDRESOURCE",
    "FINDRESOURCEEX",
    "FLOAT_RAISE",
    "FLUSH",
    "FLUSHTHREAD",
    "FMADOUBLE",
    "FMAEXTENDED",
    "FMASINGLE",
    "FPOWER10",
    "FRAC",
    "FREELIBRARY",
    "FREEMEM",
    "FREEMEMORY",
    "FREERESOURCE",
    "GET8087CW",
    "GETCPUCOUNT",
    "GETCURRENTTHREADID",
    "GETDIR",
    "GETDYNLIBSMANAGER",
    "GETFPCHEAPSTATUS",
    "GETHEAPSTATUS",
    "GETLOADERRORSTR",
    "GETMEM",
    "GETMEMORY",
    "GETMEMORYMANAGER",
    "GETMXCSR",
    "GETPROCADDRESS",
    "GETPROCEDUREADDRESS",
    "GETPROCESSID",
    "GETRESOURCEMANAGER",
    "GETSSECSR",
    "GETTEXTCODEPAGE",
    "GETTHREADID",
    "GETTHREADMANAGER",
    "GETTYPEKIND",
    "GETUNICODESTRINGMANAGER",
    "GETVARIANTMANAGER",
    "GETWIDESTRINGMANAGER",
    "GET_CALLER_ADDR",
    "GET_CALLER_FRAME",
    "GET_CALLER_STACKINFO",
    "GET_CMDLINE",
    "GET_FRAME",
    "GET_PC_ADDR",
    "GREATERTHAN(VARIANT,VARIANT):BOOLEAN",
    "GREATERTHANOREQUAL(VARIANT,VARIANT):BOOLEAN",
    "HALT",
    "HEXSTR",
    "HI",
    "HIGH",
    "HINSTANCE",
    "INC",
    "INCLUDE",
    "INDEXBYTE",
    "INDEXCHAR",
    "INDEXCHAR0",
    "INDEXDWORD",
    "INDEXQWORD",
    "INDEXWORD",
    "INITCRITICALSECTION",
    "INITIALIZE",
    "INITIALIZEARRAY",
    "INITTHREAD",
    "INITTHREADVARS",
    "INSERT",
    "INT",
    "INTDIVIDE(VARIANT,VARIANT):VARIANT",
    "INTERLOCKEDCOMPAREEXCHANGE",
    "INTERLOCKEDCOMPAREEXCHANGE64",
    "INTERLOCKEDCOMPAREEXCHANGEPOINTER",
    "INTERLOCKEDDECREMENT",
    "INTERLOCKEDDECREMENT64",
    "INTERLOCKEDEXCHANGE",
    "INTERLOCKEDEXCHANGE64",
    "INTERLOCKEDEXCHANGEADD",
    "INTERLOCKEDEXCHANGEADD64",
    "INTERLOCKEDINCREMENT",
    "INTERLOCKEDINCREMENT64",
    "IORESULT",
    "ISDYNARRAYRECTANGULAR",
    "ISMEMORYMANAGERSET",
    "IS_INTRESOURCE",
    "KILLTHREAD",
    "LEAVECRITICALSECTION",
    "LEFTSHIFT(VARIANT,VARIANT):VARIANT",
    "LENGTH",
    "LESSTHAN(VARIANT,VARIANT):BOOLEAN",
    "LESSTHANOREQUAL(VARIANT,VARIANT):BOOLEAN",
    "LETON",
    "LN",
    "LO",
    "LOADLIBRARY",
    "LOADRESOURCE",
    "LOCKRESOURCE",
    "LOGICALAND(VARIANT,VARIANT):VARIANT",
    "LOGICALNOT(VARIANT):VARIANT",
    "LOGICALOR(VARIANT,VARIANT):VARIANT",
    "LOGICALXOR(VARIANT,VARIANT):VARIANT",
    "LONGJMP",
    "LOW",
    "LOWERCASE",
    "MAKELANGID",
    "MEMSIZE",
    "MKDIR",
    "MODULUS(VARIANT,VARIANT):VARIANT",
    "MOVE",
    "MOVECHAR0",
    "MULTIPLY(VARIANT,VARIANT):VARIANT",
    "NEGATIVE(VARIANT):VARIANT",
    "NEW",
    "NTOBE",
    "NTOLE",
    "NULL",
    "OCTSTR",
    "ODD",
    "OFS",
    "ORD",
    "PACK",
    "PARAMCOUNT",
    "PARAMSTR",
    "PI",
    "POPCNT",
    "POS",
    "POWER(VARIANT,VARIANT):VARIANT",
    "PRED",
    "PREFETCH",
    "PTR",
    "RAISELIST",
    "RANDOM",
    "RANDOMIZE",
    "READ",
    "READBARRIER",
    "READDEPENDENCYBARRIER",
    "READLN",
    "READSTR",
    "READWRITEBARRIER",
    "REAL2DOUBLE",
    "REALLOCMEM",
    "REALLOCMEMORY",
    "RELEASEEXCEPTIONOBJECT",
    "RENAME",
    "RESET",
    "RESUMETHREAD",
    "REWRITE",
    "RIGHTSHIFT(VARIANT,VARIANT):VARIANT",
    "RMDIR",
    "ROLBYTE",
    "ROLDWORD",
    "ROLQWORD",
    "ROLWORD",
    "RORBYTE",
    "RORDWORD",
    "RORQWORD",
    "RORWORD",
    "ROUND",
    "RTLEVENTCREATE",
    "RTLEVENTDESTROY",
    "RTLEVENTRESETEVENT",
    "RTLEVENTSETEVENT",
    "RTLEVENTWAITFOR",
    "RUNERROR",
    "SAFELOADLIBRARY",
    "SARINT64",
    "SARLONGINT",
    "SARSHORTINT",
    "SARSMALLINT",
    "SEEK",
    "SEEKEOF",
    "SEEKEOLN",
    "SEG",
    "SET8087CW",
    "SETCODEPAGE",
    "SETDYNLIBSMANAGER",
    "SETJMP",
    "SETLENGTH",
    "SETMEMORYMANAGER",
    "SETMULTIBYTECONVERSIONCODEPAGE",
    "SETMULTIBYTEFILESYSTEMCODEPAGE",
    "SETMULTIBYTERTLFILESYSTEMCODEPAGE",
    "SETMXCSR",
    "SETRESOURCEMANAGER",
    "SETSSECSR",
    "SETSTRING",
    "SETTEXTBUF",
    "SETTEXTCODEPAGE",
    "SETTEXTLINEENDING",
    "SETTHREADDEBUGNAME",
    "SETTHREADMANAGER",
    "SETUNICODESTRINGMANAGER",
    "SETVARIANTMANAGER",
    "SETWIDESTRINGMANAGER",
    "SHORTCOMPARETEXT",
    "SIN",
    "SIZEOF",
    "SIZEOFRESOURCE",
    "SLICE",
    "SPACE",
    "SPTR",
    "SQR",
    "SQRT",
    "SSEG",
    "STACKTOP",
    "STR",
    "STRINGCODEPAGE",
    "STRINGELEMENTSIZE",
    "STRINGOFCHAR",
    "STRINGREFCOUNT",
    "STRINGTOPPCHAR",
    "STRINGTOUNICODECHAR",
    "STRINGTOWIDECHAR",
    "STRLEN",
    "STRPAS",
    "SUBTRACT(VARIANT,VARIANT):VARIANT",
    "SUCC",
    "SUSPENDTHREAD",
    "SWAP",
    "SWAPENDIAN",
    "SYSALLOCMEM",
    "SYSASSERT",
    "SYSBACKTRACESTR",
    "SYSFLUSHSTDIO",
    "SYSFREEMEM",
    "SYSFREEMEMSIZE",
    "SYSGETFPCHEAPSTATUS",
    "SYSGETHEAPSTATUS",
    "SYSGETMEM",
    "SYSINITEXCEPTIONS",
    "SYSINITFPU",
    "SYSINITSTDIO",
    "SYSMEMSIZE",
    "SYSREALLOCMEM",
    "SYSRESETFPU",
    "SYSSETCTRLBREAKHANDLER",
    "SYSTRYRESIZEMEM",
    "THREADGETPRIORITY",
    "THREADSETPRIORITY",
    "THREADSWITCH",
    "TOSINGLEBYTEFILESYSTEMENCODEDFILENAME",
    "TRUNC",
    "TRUNCATE",
    "TRYENTERCRITICALSECTION",
    "TYPEINFO",
    "TYPEOF",
    "UCS4STRINGTOUNICODESTRING",
    "UCS4STRINGTOWIDESTRING",
    "UNASSIGNED",
    "UNICODECHARLENTOSTRING",
    "UNICODECHARLENTOSTRVAR",
    "UNICODECHARTOSTRING",
    "UNICODECHARTOSTRVAR",
    "UNICODESTRINGTOUCS4STRING",
    "UNICODETOUTF8",
    "UNIQUESTRING",
    "UNLOADLIBRARY",
    "UNLOCKRESOURCE",
    "UNPACK",
    "UPCASE",
    "UTF8CODEPOINTLEN",
    "UTF8DECODE",
    "UTF8ENCODE",
    "UTF8TOANSI",
    "UTF8TOSTRING",
    "UTF8TOUNICODE",
    "VAL",
    "VARARRAYGET",
    "VARARRAYPUT",
    "VARARRAYREDIM",
    "VARCAST",
    "WAITFORTHREADTERMINATE",
    "WIDECHARLENTOSTRING",
    "WIDECHARLENTOSTRVAR",
    "WIDECHARTOSTRING",
    "WIDECHARTOSTRVAR",
    "WIDESTRINGTOUCS4STRING",
    "WRITE",
    "WRITEBARRIER",
    "WRITELN",
    "WRITESTR",
    0
};
