QT       += core
QT       -= gui

TARGET = FreePascal
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..
DEFINES += _DEBUG

SOURCES += main.cpp \
    FpLexer.cpp \
    FpPpLexer.cpp \
    FpToken.cpp \
    FpTokenType.cpp \
    CdTokenType.cpp

HEADERS += \
    FpLexer.h \
    FpToken.h \
    FpPpLexer.h \
    FpTokenType.h \
    CdTokenType.h
