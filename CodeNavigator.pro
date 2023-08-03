QT       += core gui widgets

TARGET = FpCodeNavigator
TEMPLATE = app

INCLUDEPATH +=  ..

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

!win32{
QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

DEFINES += _FP_HAVE_FILESYSTEM_

HEADERS += \
    FpLexer.h \
    FpSynTree.h \
    FpToken.h \
    FpTokenType.h \
    FpHighlighter.h \
    FpCodeNavigator.h \
    FpCodeModel.h \
    FpParser.h \
    FpRowCol.h \
    FpPpLexer.h \
    FpFileSystem.h \
    CdTokenType.h

SOURCES += \
    FpLexer.cpp \
    FpSynTree.cpp \
    FpTokenType.cpp \
    FpHighlighter.cpp \
    FpCodeNavigator.cpp \
    FpCodeModel.cpp \
    FpParser.cpp \
    FpToken.cpp \
    FpPpLexer.cpp \
    FpFileSystem.cpp \
    CdTokenType.cpp

RESOURCES += \
    CodeNavigator.qrc




