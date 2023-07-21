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

#include <QCoreApplication>
#include <QFile>
#include <QStringList>
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QElapsedTimer>
#include "FpLexer.h"
#include "FpPpLexer.h"
#include "FpParser.h"
using namespace Fp;

//#define PROCESS_ALL_FILES

QStringList collectFiles( const QDir& dir, const QStringList& suffix )
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

#ifdef PROCESS_ALL_FILES
    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ), suffix );
#endif

    files = dir.entryList( suffix, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append(dir.absoluteFilePath(f));
    }
    return res;
}

static QString root;

static void checkTokens(const QStringList& files)
{
    // works for pas/pp/inc in compiler directory after removing file with Git merge comments and fixing an non-terminated string
    // works for pas/pp in rtl directory; inc has issues with invalid asm regions
    // works in principle also for pas/pp in packages directory when fixing non-terminated strings and some strange inline #defines
    foreach( const QString& file, files )
    {
        QFile in(file);
        if( !in.open(QIODevice::ReadOnly) )
        {
            qCritical() << "cannot open file for reading:" << file;
            return;
        }
        Lexer lex;
        lex.setStream(&in, file);
        lex.setIgnoreComments(false);
        Token t = lex.nextToken();
        while(t.isValid())
        {
            qDebug() << tokenTypeName(t.d_type) << t.d_lineNr << t.d_colNr << t.d_val.simplified().left(80);
            if( t.d_type == Tok_Directive )
            {
                //qDebug() << "directive" << file.mid(root.size()+1) << t.d_lineNr << t.d_colNr << t.d_val;
                // qDebug() << t.d_val.constData();
            }
            t = lex.nextToken();
        }
        if( !t.isEof() )
        {
            qCritical() << "error at" << t.d_sourcePath << t.d_lineNr << t.d_colNr << ":" << t.d_val;
            return;
        }
    }
}

static void checkPp(const QStringList& files)
{
    foreach( const QString& file, files )
    {
        PpLexer lex;
        lex.setSearchPaths(QStringList() << root);
        lex.reset(file);
        Token t = lex.nextToken();
        while(t.isValid())
        {
            qDebug() << tokenTypeName(t.d_type) << t.d_lineNr << t.d_colNr << t.d_val.simplified().left(80);
            t = lex.nextToken();
        }
        if( !t.isEof() )
        {
            qCritical() << "error at" << t.d_sourcePath << t.d_lineNr << t.d_colNr << ":" << t.d_val;
        }
    }
}

static void dump(QTextStream& out, const SynTree* node, int level)
{
    QByteArray str;
    if( node->d_tok.d_type == Tok_Invalid )
        level--;
    else if( node->d_tok.d_type < SynTree::R_First )
    {
        if( tokenTypeIsKeyword( node->d_tok.d_type ) )
            str = tokenTypeString(node->d_tok.d_type);
        else if( node->d_tok.d_type > TT_Specials )
            str = QByteArray("\"") + node->d_tok.d_val + QByteArray("\"");
        else
            str = QByteArray("\"") + tokenTypeString(node->d_tok.d_type) + QByteArray("\"");

    }else
        str = SynTree::rToStr( node->d_tok.d_type );
    if( !str.isEmpty() )
    {
        str += QByteArray("\t") + QFileInfo(node->d_tok.d_sourcePath).baseName().toUtf8() +
                ":" + QByteArray::number(node->d_tok.d_lineNr) +
                ":" + QByteArray::number(node->d_tok.d_colNr);
        QByteArray ws;
        for( int i = 0; i < level; i++ )
            ws += "|  ";
        str = ws + str;
        out << str.data() << endl;
    }
    foreach( SynTree* sub, node->d_children )
        dump( out, sub, level + 1 );
}

static void checkParser(const QStringList& files)
{
    int ok = 0;
    class Lex : public Scanner
    {
    public:
        PpLexer lex;
        Token next()
        {
            return lex.nextToken();
        }

        Token peek(int offset)
        {
            return lex.peekToken(offset);
        }
    };

    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, files )
    {
        Lex lex;
        lex.lex.setSearchPaths(QStringList() << root);
        lex.lex.reset(file);
        Parser p(&lex);
        qDebug() << "**** parsing" << file.mid(root.size()+1);
        p.RunParser();
        if( !p.errors.isEmpty() )
        {
            foreach( const Parser::Error& e, p.errors )
                qCritical() << e.path.mid(root.size()+1) << e.row << e.col << e.msg;
#ifndef PROCESS_ALL_FILES
            break;
#endif
        }else
        {
            ok++;
            qDebug() << "ok";
        }
#if 0
        QFile out(file + ".st");
        out.open(QIODevice::WriteOnly);
        QTextStream s(&out);
        dump(s,&p.d_root,0);
#endif
    }
    qDebug() << "#### finished with" << ok << "files ok of total" << files.size() << "files" << "in" << timer.elapsed() << " [ms]";
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    if( a.arguments().size() <= 1 )
        return -1;

    QStringList files;
    QFileInfo info(a.arguments()[1]);
    if( info.isDir() )
    {
        files = collectFiles(info.filePath(), QStringList() << "*.pas" << "*.pp");
        root = info.filePath();
    }else
        files.append(info.filePath());

    //checkTokens(files);
    //checkPp(files);
    checkParser(files);

    return 0;
}
