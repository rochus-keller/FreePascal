/*
** Copyright (C) 2023 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the FreePascal project.
**
** $QT_BEGIN_LICENSE:LGPL21$
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

// migrated from LisaPascal project

#include "FpPpLexer.h"
#include "CdTokenType.h"
#include <QBuffer>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QtDebug>
using namespace Fp;

static const char* s_defined = 0;
static const char* s_undefined = 0;
static const char* s_sizeof = 0;
static const char* s_declared = 0;

PpLexer::PpLexer():d_sloc(0)
{
    if( s_defined == 0 )
    {
        s_defined = Token::toId("defined");
        s_undefined = Token::toId("undefined");
        s_sizeof = Token::toId("sizeof");
        s_declared = Token::toId("declared");
    }
}

PpLexer::~PpLexer()
{
    for( int i = 0; i < d_files.size(); i++ )
        delete d_files[i];
}

bool PpLexer::reset(const QString& filePath)
{
    d_stack.clear();
    d_buffer.clear();
    for( int i = 0; i < d_files.size(); i++ )
        delete d_files[i];
    d_files.clear();
    d_sloc = 0;
    d_includes.clear();

    d_stack.push_back(Level());
    d_stack.back().d_lex.setIgnoreComments(false);
    QFile* file = new QFile(filePath);
    if( !file->open(QIODevice::ReadOnly) )
    {
        delete file;
        d_stack.pop_back();
        return false;
    }
    d_stack.back().d_lex.setStream(file,filePath);
    return true;
}

Token PpLexer::nextToken()
{
    Token t;
    if( !d_buffer.isEmpty() )
    {
        t = d_buffer.first();
        d_buffer.pop_front();
    }else
        t = nextTokenImp();
    Q_ASSERT( t.d_type != Tok_Directive && t.d_type != Tok_Comment );
    return t;
}

Token PpLexer::peekToken(quint8 lookAhead)
{
    Q_ASSERT( lookAhead > 0 );
    while( d_buffer.size() < lookAhead )
    {
        Token t = nextTokenImp();
        Q_ASSERT( t.d_type != Tok_Comment && t.d_type != Tok_Comment );
        d_buffer.push_back( t );
    }
    return d_buffer[ lookAhead - 1 ];
}

bool PpLexer::addMacro(const QByteArray& name, const QByteArray& macro)
{
    Macro m = readMacro( macro );
    for( int i = 0; i < m.size(); i++ )
    {
        if( m[i].d_type == Tok_Invalid )
            return false;
    }
    d_macros[Token::toId(name)] = m;
    return true;
}

Token PpLexer::nextTokenImp()
{
    if( d_stack.isEmpty() )
        return Token(Tok_Eof);
    Token t = d_stack.back().d_lex.nextToken();
    while( t.d_type == Tok_Directive || t.d_type == Tok_Comment || t.d_type == Tok_Eof )
    {
        const bool statusBefore = ppthis().open;
        // qDebug() << statusBefore << t.d_lineNr << t.d_val;
        if( t.d_type == Tok_Eof )
        {
            d_files.removeAll(d_stack.back().d_lex.getDevice());
            delete d_stack.back().d_lex.getDevice();
            d_sloc += d_stack.back().d_lex.getSloc();
            d_mutes.insert(t.d_sourcePath, d_stack.back().d_mutes);
            d_stack.pop_back();
            if( d_stack.isEmpty() )
                return t;
        }
        if( t.d_type == Tok_Directive )
        {
            QByteArray data = t.d_val;
            int pos = 2;
            Cd::TokenType cd = Cd::tokenTypeFromString( data.toUpper(), &pos );
            if( cd == Cd::Tok_I && data.length() >= 4 && ( data[3] == '+' || data[3] == '-' ) )
            {
                cd = Cd::Tok_IOCHECKS;
                if( data[3] == '+' )
                    data = "ON";
                else
                    data = "OFF";
            }
            else if( cd == Cd::Tok_H && data.length() >= 4 && ( data[3] == '+' || data[3] == '-' ) )
            {
                cd = Cd::Tok_LONGSTRINGS;
                if( data[3] == '+' )
                    data = "ON";
                else
                    data = "OFF";
            }
            else if( cd != Cd::Tok_Invalid )
            {
                const bool ok = data.length() > pos && ( ::isspace(data[pos]) || data[pos] == '}' );
                if( ok )
                    data = data.mid(pos+1, data.length() - pos - 2);
                else
                    cd = Cd::Tok_Invalid;
            }
            bool ok = true;
            switch( cd )
            {
            case Cd::Tok_IF:
                ok = handleIf(data);
                break;
            case Cd::Tok_IFDEF:
                ok = handleIfdef(data);
                break;
            case Cd::Tok_IFNDEF:
                ok = handleIfndef(data);
                break;
            case Cd::Tok_ELSEIF:
                ok = handleElseif(data);
                break;
            case Cd::Tok_ELSE:
                ok = handleElse();
                break;
            case Cd::Tok_ENDIF:
                ok = handleEndif();
                break;
            case Cd::Tok_I:
            case Cd::Tok_INCLUDE:
                // TODO: include %DATE%
                if( statusBefore )
                    ok = handleInclude(data,t);
                break;
            case Cd::Tok_DEFINE:
                if( statusBefore )
                    ok = handleDefine(data);
                break;
            case Cd::Tok_MACRO:
                if( statusBefore )
                    qWarning() << "MACRO substitution not implemented" << d_stack.back().d_lex.getFilePath(); // TODO
                break;
            default:
                /* TODO The FP 3.2.2 compiler source uses in addition:
                    APPTYPE
                    ASMMODE
                    GOTO
                    INLINE
                    IOCHECKS
                    LONGSTRINGS
                    MODE
                    PACKENUM
                    PUSH
                    POP
                    */
#if 0
                if( statusBefore && cd == Cd::Tok_Invalid )
                    qWarning() << "unknown macro" << data;
#endif
                break;
            }

            if( !ok )
            {
                Token err(Tok_Invalid,t.d_lineNr,t.d_colNr,d_err.toUtf8());
                err.d_sourcePath = t.d_sourcePath;
                return err;
            }
        }
        const bool statusAfter = ppthis().open;
        if( statusBefore != statusAfter )
        {
            if( !ppthis().open )
            {
                d_startMute = t.toLoc();
                d_startMute.d_col += t.d_val.size();
            }else
                d_stack.back().d_mutes.append(qMakePair(d_startMute,t.toLoc()));
        }
        if( !ppthis().open )
        {
            t = d_stack.back().d_lex.peekToken();
            while( t.d_type != Tok_Comment && t.d_type != Tok_Directive && t.d_type != Tok_Eof )
            {
                t = d_stack.back().d_lex.nextToken();
                t = d_stack.back().d_lex.peekToken();
            }
        }
        t = d_stack.back().d_lex.nextToken();
    }
    return t;
}

bool PpLexer::handleInclude(const QByteArray& data, const Token& t)
{
    QString path;
    QFileInfo info( data.trimmed() );
    if( info.isRelative() )
    {
        const QDir dir = QFileInfo( d_stack.last().d_lex.getFilePath()).absoluteDir();
        path = dir.absoluteFilePath(info.filePath());
        if( !QFileInfo(path).exists() )
        {
            path.clear();
            foreach( const QString& dir, d_searchPaths )
            {
                path = QDir(dir).absoluteFilePath(info.filePath());
                if( QFileInfo(path).exists())
                    break;
                else
                    path.clear();
            }
        }
    }else
        path = info.absoluteFilePath();

    Include inc;
    inc.d_loc = t.toLoc();
    inc.d_sourcePath = t.d_sourcePath;
    inc.d_len = t.d_val.size();
    d_includes.append(inc);
    if( !path.isEmpty() )
    {
        d_stack.push_back(Level());
        d_stack.back().d_lex.setIgnoreComments(false);
        QFile* file = new QFile(path);
        if( !file->open(QIODevice::ReadOnly) )
        {
            delete file;
            d_stack.pop_back();
            d_err = QString("file '%1' cannot be opened").arg(info.filePath()).toUtf8();
            return false;
        }
        d_stack.back().d_lex.setStream(file,path);
    }else
    {
        d_err = QString("include file '%1' not found").arg(info.filePath()).toUtf8();
        return false;
    }
    return true;
}

class PpEval
{
public:
    PpEval(const PpLexer::Macro& m, const PpLexer::Macros& mm):d_macro(m),d_macros(mm),d_pos(0){}
    bool eval()
    {
        try
        {
            d_res = expr();
            if( nextToken().d_type != Tok_Eof )
            {
                d_err = "unexpected tokens after expression";
                return false;
            }else
                return true;
        }catch(...)
        {
            return false;
        }
    }

    const QByteArray& getErr() const { return d_err; }
    int getRes() const { return d_res; }
protected:
    const PpLexer::MacroToken& nextToken()
    {
        static PpLexer::MacroToken eof(Tok_Eof);
        if( d_pos < d_macro.size() )
            return d_macro[d_pos++];
        else
            return eof;
    }
    const PpLexer::MacroToken& peekToken() const
    {
        static PpLexer::MacroToken eof(Tok_Eof);
        if( d_pos < d_macro.size() )
            return d_macro[d_pos];
        else
            return eof;
    }
    void error(const QString& msg)
    {
        d_err = msg.toUtf8();
        throw 1;
    }

    static bool isRel(int op)
    {
        switch(op)
        {
        case Tok_Eq:
        case Tok_LtGt:
        case Tok_Lt:
        case Tok_Leq:
        case Tok_Gt:
        case Tok_Geq:
            return true;
        default:
            return false;
        }
    }
    bool expr()
    {
        QVariant res = simple_expr();
        PpLexer::MacroToken t = peekToken();
        if( isRel(t.d_type) )
        {
            t = nextToken();
            QVariant rhs = simple_expr();
            switch(t.d_type)
            {
            case Tok_Eq:
                res = (res == rhs);
                break;
            case Tok_LtGt:
                res = (res != rhs);
                break;
            case Tok_Lt:
                res = (res < rhs);
                break;
            case Tok_Leq:
                res = (res <= rhs);
                break;
            case Tok_Gt:
                res = (res > rhs);
                break;
            case Tok_Geq:
                res = (res >= rhs);
                break;
            default:
                error(QString("unexpected operator '%1' in expr").arg(tokenTypeString(t.d_type)));
                // TODO Tok_in
            }
            t = peekToken();
        }
        return asBool(res);
    }
    static bool isAdd(int op)
    {
        switch(op)
        {
        case Tok_or:
            return true;
        default:
            return false;
        }
    }
    QVariant simple_expr()
    {
        QVariant res = term();
        PpLexer::MacroToken t = peekToken();
        while( isAdd(t.d_type) )
        {
            t = nextToken();
            const bool rhs = asBool(term());
            switch(t.d_type)
            {
            case Tok_or:
                res = ( asBool(res) || rhs);
                break;
            default:
                error(QString("unexpected operator '%1' in term").arg(tokenTypeString(t.d_type)));
            }
            t = peekToken();
        }
        return res;
    }
    static bool isMult(int op)
    {
        switch(op)
        {
        case Tok_and:
            return true;
        default:
            return false;
        }
    }
    QVariant term()
    {
        QVariant res = factor();
        PpLexer::MacroToken t = peekToken();
        while( isMult(t.d_type) )
        {
            t = nextToken();
            const bool rhs = asBool(factor());
            switch(t.d_type)
            {
            case Tok_and:
                res = (asBool(res) && rhs);
                break;
            default:
                error(QString("unexpected operator '%1' in term").arg(tokenTypeString(t.d_type)));
            }
            t = peekToken();
        }
        return res;
    }
    bool call(const char* id)
    {
        PpLexer::MacroToken t = nextToken();
        if( t.d_type != Tok_Lpar )
            error(QString("expecting '('")); // TODO: FP supports these calls without ()
        t = nextToken();
        bool res = false;
        if( id == s_defined )
            res = d_macros.contains(t.d_id);
        else if( id == s_undefined )
            res = !d_macros.contains(t.d_id);
        t = nextToken();
        if( t.d_type != Tok_Rpar )
            error(QString("expecting ')'"));
        return res;
    }
    static bool asBool( const QVariant& v )
    {
        switch( v.type() )
        {
        case QVariant::ULongLong:
            return v.toULongLong() != 0;
        case QVariant::Double:
            return v.toDouble() != 0.0;
        case QVariant::Bool:
            return v.toBool();
        case QVariant::ByteArray:
            {
                const QByteArray str = v.toByteArray();
                if( str.isEmpty() || str == "0" )
                    return false;
                else
                    return true;
            }
        default:
            return false;
        }
    }
    QVariant factor()
    {
        PpLexer::MacroToken t = nextToken();
        switch( t.d_type )
        {
        case Tok_decimal_int:
            return t.d_val.toULongLong();
        case Tok_hex_int:
            return t.d_val.mid(1).toULongLong(0,16);
        case Tok_unsigned_real:
            return t.d_val.toDouble();
        case Tok_octal_int:
            return t.d_val.mid(1).toULongLong(0,8);
        case Tok_binary_int:
            return t.d_val.mid(1).toULongLong(0,2);
        case Tok_quoted_string:
            return t.d_val.mid(1, t.d_val.size()-2);
        case Tok_ident:
            {
                if( t.d_id != s_defined && t.d_id != s_undefined && t.d_id != s_sizeof
                        && t.d_id != s_declared )
                    return QVariant(); // it is legal to evaluate non-existing macros
                    // error(QString("cannot evaluate '%1'").arg(t.d_id));
                return call(t.d_id);
            }
            break;
        case Tok_Lpar:
            {
                const int res = expr();
                t = nextToken();
                if( t.d_type != Tok_Rpar )
                    error("expecting ')' after '(' in factor");
                return res;
            }
            break;
        case Tok_not:
            {
                const QVariant res = factor();
                return !asBool(res);
            }
            break;
        default:
            error(QString("factor '%1' not supported").arg(tokenTypeString(t.d_type)));
        }
        return int();
    }
private:
    const PpLexer::Macros& d_macros;
    PpLexer::Macro d_macro;
    int d_pos;
    QByteArray d_err;
    int d_res;
};

static inline bool isPseudoIdent( const PpLexer::MacroToken& t )
{
    return tokenTypeIsKeyword(t.d_type) &&
                    t.d_type != Tok_and && t.d_type != Tok_or &&
                    t.d_type != Tok_not && t.d_type != Tok_in;
}

bool PpLexer::handleDefine(const QByteArray& data)
{
    Macro m = readMacro( data );
    if( m.isEmpty() )
        return error("$DEFINE requires at least a name");
    if( m.first().d_type != Tok_ident && !isPseudoIdent(m.first()) )
        return error("identifier expected for macro name");
    if( m.size() > 1 )
    {
        if( m[1].d_type != Tok_ColonEq )
            return error("':=' expected after identifier in macro definition");
            // TODO: libndsfpc, libogcfpc, pasjpeg and sdl define macros with parameters, which is not in the spec!
        for( int i = 2; i < m.size(); i++ )
        {
            if( m[i].d_type == Tok_Invalid )
                return error(m[i].d_val);
            // we accept any token here because any could be inserted into the code
            // we delegate checks to the evaluator
        }
        d_macros[m.first().d_id] = m.mid(2);
    }
    return true;
}

bool PpLexer::handleIf(const QByteArray& data)
{
    Macro m = readMacro( data );
    if( m.isEmpty() )
        return error("$IF requires an expression");
    m = resolveRecursive(m);

    PpEval e(m,d_macros);
    if( !e.eval() )
        return error(QString("%1 in $IF expression").arg(e.getErr().constData()));

    const bool cond = e.getRes();
    d_conditionStack.append( ppstatus(false) );
    ppsetthis( ppouter().open && cond );
    return true;
}

bool PpLexer::handleElseif(const QByteArray& data)
{
    if( ppthis().elseSeen || d_conditionStack.isEmpty() )
        return error("$ELSEIF not expected here");
    Macro m = readMacro( data );
    if( m.isEmpty() )
        return error("$ELSEIF requires an expression");
    m = resolveRecursive(m);

    PpEval e(m,d_macros);
    if( !e.eval() )
        return error(QString("%1 in $ELSEIF expression").arg(e.getErr().constData()));

    const bool cond = e.getRes();
    ppsetthis( ppouter().open && cond && !ppthis().openSeen );
    return true;
}

bool PpLexer::handleElse()
{
    if( ppthis().elseSeen || d_conditionStack.isEmpty() )
        return error("$ELSE not expected here");
    else
        ppsetthis( ppouter().open && !ppthis().openSeen, true );
    return true;
}

bool PpLexer::handleEndif()
{
    if( d_conditionStack.isEmpty() )
        return error("spurious $ENDIF");
    else
        d_conditionStack.pop_back();
    return true;
}

bool PpLexer::handleIfdef(const QByteArray& data)
{
    Macro m = readMacro( data );
    if( m.size() != 1 || ( m.first().d_type != Tok_ident && !isPseudoIdent(m.first())) )
        return error("$IFDEF requires a single name");

    const bool cond = d_macros.contains(m.first().d_id);
    d_conditionStack.append( ppstatus(false) );
    ppsetthis( ppouter().open && cond );
    return true;
}

bool PpLexer::handleIfndef(const QByteArray& data)
{
    Macro m = readMacro( data );
    if( m.size() != 1 || ( m.first().d_type != Tok_ident && !isPseudoIdent(m.first())) )
        return error("$IFDEF requires a single name");

    const bool cond = !d_macros.contains(m.first().d_id);
    d_conditionStack.append( ppstatus(false) );
    ppsetthis( ppouter().open && cond );
    return true;
}

bool PpLexer::error(const QString& msg)
{
    d_err = msg;
    return false;
}

PpLexer::Macro PpLexer::readMacro(const QByteArray& str) const
{
    Lexer lex;
    lex.setIgnoreComments(true);
    lex.setCopyKeywords(true);
    QList<Token> t = lex.tokens(str);
    Macro m;
    for( int i = 0; i < t.size(); i++ )
    {
        MacroToken mt;
        mt.d_type = t[i].d_type;
        mt.d_val = t[i].d_val;
        mt.d_id = t[i].d_id;
        m << mt;
    }
    return m;
}

static inline bool isBuiltIn( const PpLexer::MacroToken& t )
{
    return t.d_type == Tok_ident && ( t.d_id == s_defined || t.d_id == s_undefined ||
            t.d_id == s_sizeof || t.d_id != s_declared );
}

PpLexer::Macro PpLexer::resolve(const PpLexer::Macro& in, bool* changed) const
{
    Macro out;
    int mods = 0;
    int lock = 0;
    for( int i = 0; i < in.size(); i++ )
    {
        if( lock )
        {
            lock--;
            out << in[i];
            continue;
        }
        const bool bi = isBuiltIn(in[i]);
        if( bi )
        {
            // dont resolve idents which are arguments of defined() and co.
            if( i+1 < in.size() && in[i+1].d_type == Tok_Lpar )
                lock = 3;
            else
                lock = 1;
            out << in[i];
        }else if( in[i].d_type == Tok_ident || isPseudoIdent(in[i]) )
        {
            Macros::const_iterator j = d_macros.find(in[i].d_id);
            if( j != d_macros.end() )
            {
                mods++;
                out << j.value();
            }else
                out << in[i];
        }else
            out << in[i];
    }
    if( changed )
        *changed = mods;
    return out;
}

PpLexer::Macro PpLexer::resolveRecursive(const PpLexer::Macro& m) const
{
    Macro out = m;
    bool changed = true;
    while( changed )
        out = resolve(out,&changed);
    return out;
}

