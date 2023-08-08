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
// Adopted from the Lisa Pascal Navigator

#include "FpCodeModel.h"
#include "FpPpLexer.h"
#include "FpParser.h"
#include <QFile>
#include <QPixmap>
#include <QtDebug>
#include <QCoreApplication>
using namespace Fp;

#define LISA_WITH_MISSING

class PascalModelVisitor
{
    CodeModel* d_mdl;
    UnitFile* d_cf;
    QHash<Declaration*,Declaration*> d_redirect;
    struct Deferred
    {
        Symbol* sym;
        Type::Ref pointer;
        Scope* scope;
        SynTree* typeIdent;
    };
    QList<Deferred> d_deferred;
    QList<Declaration*> d_forwards;
    QList<Scope*> d_scopes;

public:
    PascalModelVisitor(CodeModel* m):d_mdl(m) {}

    void visit( UnitFile* cf, SynTree* top )
    {      
        d_cf = cf;
        cf->d_globals = d_mdl->getGlobals();
        if( top->d_children.isEmpty() )
            return;
        if( top->d_tok.d_type == Tok_Invalid )
            top = top->d_children.first();

        switch(top->d_children.first()->d_tok.d_type)
        {
        case SynTree::R_program_:
            program_(top->d_children.first());
            break;
        case SynTree::R_unit_:
            unit_(top->d_children.first());
            break;
        case SynTree::R_library_:
            library_(top->d_children.first());
            break;
        }
    }
private:

    static inline void dummy() {}

    void FreePascal(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_FreePascal);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_program_:
                program_(sub);
                break;
            case SynTree::R_unit_:
                unit_(sub);
                break;
            case SynTree::R_library_:
                library_(sub);
                break;
            }
        }
    }

    void program_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_program_);
        Scope* s = new Scope();
        s->d_owner = d_cf;
        s->d_kind = Thing::Body;
        d_cf->d_impl = s;
        d_scopes.push_back(s);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_program_header:
                program_header(sub);
                break;
            case SynTree::R_uses_clause:
                uses_clause(sub);
                break;
            case SynTree::R_block:
                block(sub);
                break;
            }
        }
        d_scopes.pop_back();
    }

    void program_header(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_program_header);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addDecl(d_cf->d_globals,sub->d_tok,Thing::Module);
                break;
            case SynTree::R_program_parameters:
                program_parameters(sub);
                break;
            }
        }
    }

    void program_parameters(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_program_parameters);
        // TODO
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                identifier_list(sub);
                break;
            }
        }
    }

    void uses_clause(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_uses_clause);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_uses_clause_:
                uses_clause_(sub);
                break;
            }
        }
    }

    void uses_clause_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_uses_clause_);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addSym(d_cf->d_globals,sub->d_tok);
                break;
            case SynTree::R_string_literal:
                // TODO
                string_literal(sub);
                break;
            }
        }
    }

    void unit_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_unit_);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_unit_header:
                unit_header(sub);
                break;
            case SynTree::R_interface_part:
                interface_part(sub);
                break;
            case SynTree::R_implementation_part:
                implementation_part(sub);
                break;
            case SynTree::R_initialization_part:
                initialization_part(sub);
                break;
            case SynTree::R_finalization_part:
                finalization_part(sub);
                break;
            case SynTree::R_statement_list:
                if( d_cf->d_impl != 0 )
                {
                    d_scopes.push_back(d_cf->d_impl);
                    statement_list(sub);
                    d_scopes.pop_back();
                }
                break;
            }
        }
    }

    void unit_header(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_unit_header);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_qualifier:
                {
                    TokenList toks = qualifier(sub);
                    Scope* outer = d_cf->d_globals;
                    for( int i = 0; i < toks.size() - 1; i++ )
                    {
                        Declaration* d = findInMembers(outer, toks[i]);
                        if( d == 0 )
                        {
                            d = addDecl(outer,toks[i],Thing::Namespace);
                            Scope* newScope = new Scope();
                            newScope->d_owner = d_cf;
                            newScope->d_kind = Thing::Body;
                            newScope->d_outer = outer;
                            d->d_body = newScope;
                        }
                        outer = d->d_body;
                    }
                    addDecl(outer,toks.last(),Thing::Module);
                }
                break;
            case SynTree::R_hintdirectives:
                hintdirectives(sub);
                break;
            }
        }
    }

    void interface_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_interface_part);
        Scope* newScope = new Scope();
        newScope->d_owner = d_cf;
        newScope->d_kind = Thing::Interface;
        d_cf->d_intf = newScope;
        d_scopes.push_back(newScope);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_uses_clause:
                uses_clause(sub);
                break;
            case SynTree::R_constant_declaration_part:
                constant_declaration_part(sub);
                break;
            case SynTree::R_type_declaration_part:
                type_declaration_part(sub);
                break;
            case SynTree::R_variable_declaration_part:
                variable_declaration_part(sub);
                break;
            case SynTree::R_property_declaration_part:
                property_declaration_part(sub);
                break;
            case SynTree::R_procedure_headers_part:
                procedure_headers_part(sub);
                break;
            }
        }
        d_scopes.pop_back();
    }

    void procedure_headers_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_procedure_headers_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_procedure_header:
                function_procedure_header(sub, Thing::Proc);
                break;
            case SynTree::R_function_header:
                function_procedure_header(sub, Thing::Func);
                break;
            case SynTree::R_operator_header:
                operator_header(sub);
                break;
            case SynTree::R_call_modifiers:
                call_modifiers(sub);
                break;
            }
        }
    }

    void implementation_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_implementation_part);
        Scope* newScope = new Scope();
        newScope->d_owner = d_cf;
        newScope->d_kind = Thing::Implementation;
        newScope->d_outer = d_cf->d_intf;
        d_cf->d_impl = newScope;
        d_scopes.push_back(newScope);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_uses_clause:
                uses_clause(sub);
                break;
            case SynTree::R_declaration_part:
                declaration_part(sub);
                break;
            }
        }
        d_scopes.pop_back();
    }

    void initialization_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_initialization_part);
        if( d_cf->d_impl == 0 )
            return;
        d_scopes.push_back(d_cf->d_impl);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement_list:
                statement_list(sub);
                break;
            }
        }
        d_scopes.pop_back();
    }

    void finalization_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_finalization_part);
        if( d_cf->d_impl == 0 )
            return;
        d_scopes.push_back(d_cf->d_impl);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement_list:
                statement_list(sub);
                break;
            }
        }
        d_scopes.pop_back();
    }

    void library_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_library_);
        Scope* s = new Scope();
        s->d_owner = d_cf;
        s->d_kind = Thing::Body;
        d_cf->d_impl = s;
        d_scopes.push_back(s);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_library_header:
                library_header(sub);
                break;
            case SynTree::R_uses_clause:
                uses_clause(sub);
                break;
            case SynTree::R_block:
                block(sub);
                break;
            }
        }
        d_scopes.pop_back();
    }

    void library_header(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_library_header);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_library:
                break;
            case Tok_ident:
                addDecl(d_cf->d_globals,sub->d_tok,Thing::Module);
                break;
            }
        }
    }

    void exports_clause(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_exports_clause);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_exports_list:
                exports_list(sub);
                break;
            }
        }
    }

    void exports_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_exports_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_exports_entry:
                exports_entry(sub);
                break;
            }
        }
    }

    void exports_entry(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_exports_entry);

        // TODO
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                break;
            case Tok_index:
                break;
            case SynTree::R_integer_constant:
                integer_constant(sub);
                break;
            case Tok_name:
                break;
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            }
        }
    }

    void block(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_block);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_declaration_part:
                declaration_part(sub);
                break;
            case SynTree::R_statement_part:
                statement_part(sub);
                break;
            }
        }
    }

    void declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_label_declaration_part:
                label_declaration_part(sub);
                break;
            case SynTree::R_constant_declaration_part:
                constant_declaration_part(sub);
                break;
            case SynTree::R_resourcestring_declaration_part:
                resourcestring_declaration_part(sub);
                break;
            case Tok_class:
                // optional prefix for func_proc_declaration_part
                break;
            case SynTree::R_func_proc_declaration_part:
                func_proc_declaration_part(sub);
                break;
            case SynTree::R_type_declaration_part:
                type_declaration_part(sub);
                break;
            case SynTree::R_variable_declaration_part:
                variable_declaration_part(sub);
                break;
            case SynTree::R_threadvariable_declaration_part:
                threadvariable_declaration_part(sub);
                break;
            case SynTree::R_exports_clause:
                exports_clause(sub);
                break;
            }
        }
    }

    void label_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_label_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_label_def:
                label_def(sub);
                break;
            }
        }
    }

    void constant_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_constant_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_constant_declaration:
                constant_declaration(sub);
                break;
            }
        }
    }

    void resourcestring_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_resourcestring_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_resourcestring:
                break;
            case SynTree::R_string_constant_declaration:
                string_constant_declaration(sub);
                break;
            }
        }
    }

    void type_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_type_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_declaration:
                type_declaration(sub);
                break;
            }
        }
        for( int i = 0; i < d_deferred.size(); i++ )
        {
            const Deferred& def = d_deferred[i];
            if( def.sym == 0 )
                def.pointer->d_type = resolvedType(type_identifier(def.scope,def.typeIdent));
#ifdef LISA_WITH_MISSING
            else if( def.sym->d_decl == 0 && !def.typeIdent->d_children.isEmpty())
            {
                Token t = def.typeIdent->d_children.first()->d_tok;
                Declaration* d = d_scopes.back()->findDecl(t.d_id);
                if( d && d_cf->d_kind == Thing::Unit )
                {
                    QHash<Declaration*,Declaration*>::const_iterator intf = d_redirect.find(d);
                    if( intf != d_redirect.end() )
                        d = intf.value(); // attach all refs to the interface declaration (otherwise they are not visible)
                }
                def.sym->d_decl = d;
                def.pointer->d_type = resolvedType(def.sym);
                if( d )
                    d->d_refs[t.d_sourcePath].append(def.sym);
            }
#endif
        }
        d_deferred.clear();
    }

    void variable_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variable_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_variable_declaration:
                variable_declaration(sub);
                break;
            }
        }
    }

    void threadvariable_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_threadvariable_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_variable_declaration:
                variable_declaration(sub);
                break;
            }
        }
    }

    void func_proc_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_func_proc_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_procedure_declaration:
                procedure_declaration(sub);
                break;
            case SynTree::R_function_declaration:
                function_declaration(sub);
                break;
            case SynTree::R_constructor_declaration:
                constructor_declaration(sub);
                break;
            case SynTree::R_destructor_declaration:
                destructor_declaration(sub);
                break;
            case SynTree::R_operator_definition:
                operator_definition(sub);
                break;
            }
        }
    }

    void statement_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_statement_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_compound_statement:
                compound_statement(sub);
                break;
            }
        }
    }

    void hint_directive(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_hint_directive);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_deprecated:
                break;
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            case Tok_experimental:
                break;
            case Tok_platform:
                break;
            case Tok_unimplemented:
                break;
            }
        }
    }

    void hintdirectives(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_hintdirectives);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_hint_directive:
                hint_directive(sub);
                break;
            }
        }
    }

    void constant_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_constant_declaration);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addDecl( sub->d_tok, Thing::Const);
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            case SynTree::R_type_:
                type_(sub);
                break;
            case SynTree::R_typed_constant:
                typed_constant(sub);
                break;
            case SynTree::R_hintdirectives:
                hintdirectives(sub);
                break;
            }
        }
    }

    void typed_constant(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_typed_constant);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_factor:
                factor(sub);
                break;
            case Tok_procedural_constant:
                // TODO
                break;
            }
        }
    }

    void type_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_type_declaration);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                d = addDecl(sub->d_tok, Thing::TypeDecl);
                break;
            case SynTree::R_type_:
                {
                    Type::Ref t = type_(sub);
                    if( d )
                        d->d_type = t;
                }
                break;
            case SynTree::R_hintdirectives:
                hintdirectives(sub);
                break;
            case Tok_Semi:
                break;
            }
        }
    }

    Type::Ref type_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_type_);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_helper_type:
                res = helper_type(sub);
                break;
            case SynTree::R_generic_type:
                res = generic_type(sub);
                break;
            case SynTree::R_structured_type:
                res = structured_type(sub);
                break;
            case SynTree::R_specialized_type:
                res = specialized_type(sub);
                break;
            case SynTree::R_simple_type:
                res = simple_type(sub);
                break;
            case SynTree::R_pointer_type:
                res = pointer_type(sub);
                break;
            case SynTree::R_procedural_type:
                res = procedural_type(sub);
                break;
            }
        }
        return res;
    }

    Type::Ref simple_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_simple_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_string_type:
                string_type(sub);
                break;
            case SynTree::R_ordinal_type:
                res = ordinal_type(sub);
                break;
            }
        }
        return res;
    }

    Type::Ref type_name(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_type_name);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                {
                    Symbol* sym = addSym(d_scopes.back(),sub->d_tok);
                    res = resolvedType(sym);
                }
                break;
            case Tok_string:
                break;
            }
        }
        return res;
    }

    Type::Ref subrange_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_subrange_type);
        Type::Ref res;
        SynTree* ex = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_simple_constant_expression:
                ex = sub;
                // simple_constant_expression(sub);
                break;
            case Tok_2Dot:
                return res;
            }
        }
        if( ex )
        {
            while( ex && ex->d_children.size() == 1 )
                ex = ex->d_children.first();
            if( ex && ex->d_tok.d_type == Tok_ident )
            {
                Symbol* sym = addSym(d_scopes.back(),ex->d_tok);
                res = resolvedType(sym);
            }
        }
        return res;
    }

    void string_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_string_type);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_string:
                break;
            case Tok_ansistring:
                break;
            case Tok_Lbrack:
                break;
            case SynTree::R_constant_expression:
                constant_expression(sub);
                break;
            case Tok_Rbrack:
                break;
            case Tok_Lpar:
                break;
            case SynTree::R_unsigned_integer:
                unsigned_integer(sub);
                break;
            case Tok_Rpar:
                break;
            }
        }
    }

    Type::Ref ordinal_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ordinal_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_enumerated_type:
                enumerated_type(sub);
                break;
            case SynTree::R_subrange_type:
                res = subrange_type(sub);
                break;
            }
        }
        return res;
    }

    TokenList identifier_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_identifier_list);
        TokenList res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                res.append( sub->d_tok );
                break;
            }
        }
        return res;
    }

    void enumerated_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_enumerated_type);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_enumerated_type_:
                enumerated_type_(sub);
                break;
            }
        }
    }

    void enumerated_type_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_enumerated_type_);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_assigned_enum_list:
                assigned_enum_list(sub);
                break;
            }
        }
    }

    void assigned_enum_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_assigned_enum_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_assigned_enum_:
                assigned_enum_(sub);
                break;
            }
        }
    }

    void assigned_enum_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_assigned_enum_);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addDecl(sub->d_tok, Thing::Const);
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    Type::Ref structured_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_structured_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_class_reference_type:
                res = class_reference_type(sub);
                break;
            case SynTree::R_packable_type_:
                res = packable_type_(sub);
                break;
            case SynTree::R_interface_type:
                res = interface_type(sub);
                break;
            case SynTree::R_set_type:
                set_type(sub);
                break;
            case SynTree::R_file_type:
                file_type(sub);
                break;
            }
        }
        return res;
    }

    Type::Ref packable_type_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_packable_type_);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_array_type:
                res = array_type(sub);
                break;
            case SynTree::R_record_type:
                res = record_type(sub);
                break;
            case SynTree::R_object_type:
                res = object_type(sub);
                break;
            case SynTree::R_class_type:
                res = class_type(sub);
                break;
            }
        }
        return res;
    }

    Type::Ref array_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_array_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_ordinal_type:
                ordinal_type(sub);
                break;
            case SynTree::R_type_:
                res = type_(sub);
                break;
            }
        }
        if( res.data() != 0 )
        {
            Type::Ref arr( new Type() );
            arr->d_kind = Type::Array;
            arr->d_type = res;
            res = arr;
        }
        return res;
    }

    void field_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_field_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_fixed_fields:
                fixed_fields(sub);
                break;
            case SynTree::R_variant_part:
                variant_part(sub);
                break;
            }
        }
    }

    void variant_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variant_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addSym(d_scopes.back(),sub->d_tok);
                break;
            case SynTree::R_ordinal_type:
                ordinal_type(sub);
                break;
            case SynTree::R_variant:
                variant(sub);
                break;
            }
        }
    }

    void variant(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variant);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_constant:
                constant(sub);
                break;
            case SynTree::R_field_list:
                field_list(sub);
                break;
            }
        }
    }

    void fixed_fields(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_fixed_fields);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_fixed_field_:
                fixed_field_(sub);
                break;
            }
        }
    }

    void fixed_field_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_fixed_field_);
        TokenList ids;
        Type::Ref tp;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_type_:
                tp = type_(sub);
                break;
            }
        }
        foreach( const Token& t, ids )
        {
            Declaration* d = addDecl(t, Thing::Field);
            d->d_type = tp;
        }
    }

    Type::Ref record_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_record_type);
        Type::Ref res(new Type());
        res->d_kind = Type::Record;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_outer = d_scopes.back();
        d_scopes.push_back(res->d_members);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_component_list3:
                component_list3(sub);
                break;
            case SynTree::R_variant_part:
                variant_part(sub);
                break;
            }
        }
        d_scopes.pop_back();
        return res;
    }

    void component_list3(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_component_list3);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_record_visibility_specifier:
                record_visibility_specifier(sub);
                break;
            case SynTree::R_fixed_field_:
                fixed_field_(sub);
                break;
            case SynTree::R_record_method_definition:
                record_method_definition(sub);
                break;
            case SynTree::R_record_operator_definition:
                record_operator_definition(sub);
                break;
            }
        }
    }

    void record_visibility_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_record_visibility_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            // NOP
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_public:
                break;
            case Tok_strict:
                break;
            case Tok_private:
                break;
            }
        }
    }

    void record_method_definition(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_record_method_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_function_header:
                function_procedure_header(sub, Thing::Func);
                break;
            case SynTree::R_procedure_header:
                function_procedure_header(sub, Thing::Proc);
                break;
            case SynTree::R_call_modifiers:
                call_modifiers(sub);
                break;
            }
        }
    }

    void record_operator_definition(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_record_operator_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_class:
                // optional prefix
                break;
            case SynTree::R_operator_definition:
                operator_definition(sub);
                break;
            }
        }
    }

    void set_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_set_type);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_ordinal_type:
                ordinal_type(sub);
                break;
            }
        }
    }

    void file_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_file_type);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_name:
                type_name(sub);
                break;
            }
        }
    }

    void defer( Symbol* sym, Type* pointer, Scope* scope, SynTree* typeIdent )
    {
        // TODO: here we only support
        // the original Pascal scope rules
        Deferred def;
        def.sym = sym;
        def.pointer = pointer;
        def.scope = scope;
        def.typeIdent = typeIdent;
        d_deferred.append(def);
    }

    Type::Ref pointer_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_pointer_type);
        Type::Ref t;
        Symbol* sym = 0;
        SynTree* id = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_name:
                if( sub->d_children.size() == 1 )
                {
                    id = sub->d_children.first();
                    sym = addSym(d_scopes.back(),id->d_tok);
                    t = resolvedType(sym);
                }
                break;
            }
        }
        Type::Ref ptr(new Type());
        ptr->d_kind = Type::Pointer;
        ptr->d_type = t;

        if( t.data() == 0 || sym == 0 || sym->d_decl == 0 )
            defer( sym, ptr.data(), d_scopes.back(), id );
        return ptr;
    }

    Type::Ref procedural_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_procedural_type);
        Type::Ref res(new Type());
        res->d_kind = Type::Procedural;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_outer = d_scopes.back();
        d_scopes.push_back(res->d_members);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_func_proc_header:
                func_proc_header(sub);
                break;
            case Tok_of:
                break;
            case Tok_object:
                break;
            case Tok_is:
                break;
            case Tok_nested:
                break;
            case Tok_Semi:
                break;
            case SynTree::R_call_modifiers:
                call_modifiers(sub);
                break;
            }
        }
        d_scopes.pop_back();
        return res;
    }

    void func_proc_header(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_func_proc_header);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_formal_parameter_list:
                formal_parameter_list(sub);
                break;
            case SynTree::R_result_type:
                result_type(sub);
                break;
            }
        }
    }

    void call_modifiers(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_call_modifiers);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_inline:
                break;
            case Tok_register:
                break;
            case Tok_cdecl:
                break;
            case Tok_pascal:
                break;
            case Tok_stdcall:
                break;
            case Tok_safecall:
                break;
            }
        }
    }

    void variable_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variable_declaration);
        Type::Ref t;
        TokenList ids;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_type_:
                t = type_(sub);
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            case SynTree::R_variable_modifiers:
                variable_modifiers(sub);
                break;
            case SynTree::R_hintdirectives:
                hintdirectives(sub);
                break;
            }
        }
        foreach( const Token& id, ids )
        {
            Declaration* d = addDecl(id, Thing::Var);
            d->d_type = t;
        }
    }

    void variable_modifiers(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variable_modifiers);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_variable_modifier_:
                variable_modifier_(sub);
                break;
            }
        }
    }

    void variable_modifier_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variable_modifier_);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_absolute:
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            case Tok_Semi:
                break;
            case Tok_export:
                break;
            case Tok_cvar:
                break;
            case Tok_external:
                break;
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            case Tok_name:
                break;
            }
        }
    }

    void property_definition(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_property_definition);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                {
                    d = addDecl(sub->d_tok, Thing::Property);
                    d->d_body = new Scope();
                    d->d_body->d_kind = Thing::Body;
                    d->d_body->d_owner = d;
                    d->d_body->d_outer = d_scopes.back();
                }
                break;
            case SynTree::R_property_interface:
                d_scopes.push_back(d->d_body);
                property_interface(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_property_specifiers:
                d_scopes.push_back(d->d_body);
                property_specifiers(sub);
                d_scopes.pop_back();
                break;
            }
        }
    }

    void property_interface(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_property_interface);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_property_parameter_list:
                property_parameter_list(sub);
                break;
            case SynTree::R_type_name:
                type_name(sub);
                break;
            case SynTree::R_integer_constant:
                integer_constant(sub);
                break;
            }
        }
    }

    void property_parameter_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_property_parameter_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_parameter_declaration:
                parameter_declaration(sub);
                break;
            }
        }
    }

    void property_specifiers(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_property_specifiers);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_read_specifier:
                read_specifier(sub);
                break;
            case SynTree::R_write_specifier:
                write_specifier(sub);
                break;
            case SynTree::R_default_specifier:
                default_specifier(sub);
                break;
            }
        }
    }

    void read_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_read_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_read:
                break;
            case SynTree::R_field_or_function:
                field_or_function(sub);
                break;
            }
        }
    }

    void write_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_write_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_field_or_procedure:
                field_or_procedure(sub);
                break;
            }
        }
    }

    void default_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_default_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_constant:
                constant(sub);
                break;
            }
        }
    }

    void field_or_function(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_field_or_function);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_qualifier:
                resolveMember( qualifier(sub), d_scopes.back() );
                break;
            }
        }
    }

    void field_or_procedure(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_field_or_procedure);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_qualifier:
                resolveMember( qualifier(sub), d_scopes.back() );
                break;
            }
        }
    }

    Type::Ref object_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_object_type);
        Type::Ref res(new Type());
        res->d_kind = Type::Class;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_outer = d_scopes.back();
        d_scopes.push_back(res->d_members);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_heritage:
                {
                    Type::Ref super = heritage(sub);
                    if( super && super->d_kind == Type::Class )
                    {
                        res->d_type = super;
                        res->d_members->d_altOuter = d_scopes.back();
                        res->d_members->d_outer = super->d_members;
                    }
                }
                break;
            case SynTree::R_component_list:
                component_list(sub);
                break;
            }
        }
        d_scopes.pop_back();
        return res;
    }

    Type::Ref heritage(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_heritage);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_name:
                res = type_name(sub);
                break;
            }
        }
        return res;
    }

    void component_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_component_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_object_visibility_specifier:
                object_visibility_specifier(sub);
                break;
            case SynTree::R_const_definition:
                const_definition(sub);
                break;
            case SynTree::R_field_definition:
                field_definition(sub);
                break;
            case SynTree::R_method_definition:
                method_definition(sub);
                break;
            case SynTree::R_property_declaration_part:
                property_declaration_part(sub);
                break;
            }
        }
    }

    void field_definition(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_field_definition);
        TokenList ids;
        Type::Ref tp;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_type_:
                tp = type_(sub);
                break;
            }
        }
        foreach( const Token& t, ids )
        {
            Declaration* d = addDecl(t, Thing::Field);
            d->d_type = tp;
        }
    }

    void const_definition(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_const_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addDecl(sub->d_tok, Thing::Const);
                break;
            case SynTree::R_constant_expression:
                constant_expression(sub);
                break;
            }
        }
    }

    void object_visibility_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_object_visibility_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_private:
                break;
            case Tok_protected:
                break;
            case Tok_public:
                break;
            }
        }
    }

    void constructor_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_constructor_declaration);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_constructor_header:
                d = constructor_header(sub, true);
                break;
            case SynTree::R_subroutine_block:
                if( d )
                    d_scopes.push_back(d->d_body);
                subroutine_block(sub);
                if( d )
                    d_scopes.pop_back();
                break;
            }
        }
    }

    void destructor_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_destructor_declaration);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_destructor_header:
                d = destructor_header(sub, true);
                break;
            case Tok_Semi:
                break;
            case SynTree::R_subroutine_block:
                if( d )
                    d_scopes.push_back(d->d_body);
                subroutine_block(sub);
                if( d )
                    d_scopes.pop_back();
                break;
            }
        }
    }

    Declaration* constructor_header(SynTree* st, bool impl = false) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_constructor_header);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_qualifier:
                d = createProcDecl(qualifier(sub), Thing::Proc, impl);
                break;
            case SynTree::R_formal_parameter_list:
                d_scopes.push_back(d->d_body);
                formal_parameter_list(sub);
                d_scopes.pop_back();
                break;
            }
        }
        return d;
    }

    Declaration* destructor_header(SynTree* st, bool impl = false) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_destructor_header);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_qualifier:
                d = createProcDecl(qualifier(sub),Thing::Proc,impl);
                break;
            case SynTree::R_formal_parameter_list:
                d_scopes.push_back(d->d_body);
                formal_parameter_list(sub);
                d_scopes.pop_back();
                break;
            }
        }
        return d;
    }

    void method_definition(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_method_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_function_header:
                function_procedure_header(sub, Thing::Func);
                break;
            case SynTree::R_procedure_header:
                function_procedure_header(sub, Thing::Proc);
                break;
            case SynTree::R_constructor_header:
                constructor_header(sub);
                break;
            case SynTree::R_destructor_header:
                destructor_header(sub);
                break;
            case SynTree::R_method_directives:
                method_directives(sub);
                break;
            }
        }
    }

    void method_directives(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_method_directives);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            // NOP
            case Tok_virtual:
                break;
            case Tok_Semi:
                break;
            case Tok_abstract:
                break;
            case Tok_reintroduce:
                break;
            case SynTree::R_call_modifiers:
                call_modifiers(sub);
                break;
            }
        }
    }

    Type::Ref class_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_class_type);
        Type::Ref res(new Type());
        res->d_kind = Type::Class;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_outer = d_scopes.back();
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_heritage2:
                {
                    Type::Ref super = heritage2(sub);
                    if( super && super->d_kind == Type::Class )
                    {
                        res->d_type = super;
                        res->d_members->d_altOuter = d_scopes.back();
                        res->d_members->d_outer = super->d_members;
                    }
                }
                break;
            case SynTree::R_component_list2:
                d_scopes.push_back(res->d_members);
                component_list2(sub);
                d_scopes.pop_back();
                break;
            }
        }
        return res;
    }

    Type::Ref heritage2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_heritage2);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_name:
                res = type_name(sub);
                break;
            case SynTree::R_implemented_interfaces:
                // TODO: is this scope required?
                implemented_interfaces(sub);
                break;
            }
        }
        return res;
    }

    void implemented_interfaces(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_implemented_interfaces);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addSym(d_scopes.back(),sub->d_tok);
                break;
            }
        }
    }

    void component_list2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_component_list2);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_class_visibility_specifier:
                class_visibility_specifier(sub);
                break;
            case SynTree::R_field_definition2:
                field_definition2(sub);
                break;
            case SynTree::R_class_part:
                class_part(sub);
                break;
            }
        }
    }

    void class_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_class_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_constant_declaration_part:
                constant_declaration_part(sub);
                break;
            case SynTree::R_type_declaration_part:
                type_declaration_part(sub);
                break;
            case SynTree::R_variable_declaration_part:
                variable_declaration_part(sub);
                break;
            case SynTree::R_method_definition2:
                method_definition2(sub);
                break;
            case SynTree::R_property_definition2:
                property_definition2(sub);
                break;
            }
        }
    }

    void class_visibility_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_class_visibility_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_public:
                break;
            case Tok_published:
                break;
            case Tok_strict:
                break;
            case Tok_private:
                break;
            case Tok_protected:
                break;
            }
        }
    }

    void method_definition2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_method_definition2);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_function_header:
                function_procedure_header(sub, Thing::Func);
                break;
            case SynTree::R_procedure_header:
                function_procedure_header(sub, Thing::Proc);
                break;
            case SynTree::R_constructor_header:
                constructor_header(sub);
                break;
            case SynTree::R_destructor_header:
                destructor_header(sub);
                break;
            case SynTree::R_method_directives2:
                method_directives2(sub);
                break;
            case SynTree::R_call_modifiers:
                call_modifiers(sub);
                break;
            }
        }
    }

    void method_directives2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_method_directives2);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            // NOP
            case Tok_virtual:
                break;
            case Tok_dynamic:
                break;
            case Tok_Semi:
                break;
            case Tok_abstract:
                break;
            case Tok_overload:
                break;
            case Tok_reintroduce:
                break;
            case Tok_override:
                break;
            case Tok_final:
                break;
            case Tok_message:
                break;
            case SynTree::R_integer_constant:
                integer_constant(sub);
                break;
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            case Tok_static:
                break;
            case Tok_inline:
                break;
            }
        }
    }

    void field_definition2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_field_definition2);
        TokenList ids;
        Type::Ref tp;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_type_:
                tp = type_(sub);
                break;
            }
        }
        foreach( const Token& t, ids )
        {
            Declaration* d = addDecl(t, Thing::Field);
            d->d_type = tp;
        }
    }

    void property_definition2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_property_definition2);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                {
                    d = addDecl(sub->d_tok, Thing::Property);
                    d->d_body = new Scope();
                    d->d_body->d_kind = Thing::Body;
                    d->d_body->d_owner = d;
                    d->d_body->d_outer = d_scopes.back();
                }
                break;
            case SynTree::R_property_interface:
                d_scopes.push_back(d->d_body);
                property_interface(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_property_specifiers2:
                d_scopes.push_back(d->d_body);
                property_specifiers2(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_hintdirectives:
                hintdirectives(sub);
                break;
            }
        }
    }

    void property_specifiers2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_property_specifiers2);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_read_specifier:
                read_specifier(sub);
                break;
            case SynTree::R_write_specifier:
                write_specifier(sub);
                break;
            case SynTree::R_implements_specifier:
                implements_specifier(sub);
                break;
            case SynTree::R_default_specifier:
                default_specifier(sub);
                break;
            case SynTree::R_stored_specifier:
                stored_specifier(sub);
                break;
            case SynTree::R_defaultarraypropertyspecifier:
                defaultarraypropertyspecifier(sub);
                break;
            }
        }
    }

    void implements_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_implements_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addSym(d_scopes.back(),sub->d_tok);
                break;
            }
        }
    }

    void stored_specifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_stored_specifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_stored:
                break;
            case SynTree::R_constant:
                constant(sub);
                break;
            }
        }
    }

    void defaultarraypropertyspecifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_defaultarraypropertyspecifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            // NOP
            case Tok_default:
                break;
            }
        }
    }

    Type::Ref interface_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_interface_type);
        Type::Ref res(new Type());
        res->d_kind = Type::Class;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_outer = d_scopes.back();
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_heritage:
                {
                    Type::Ref super = heritage(sub);
                    if( super && super->d_kind == Type::Class )
                    {
                        res->d_type = super;
                        res->d_members->d_altOuter = d_scopes.back();
                        res->d_members->d_outer = super->d_members;
                    }
                }
                break;
            case SynTree::R_guid:
                guid(sub);
                break;
            case SynTree::R_component_list2:
                d_scopes.push_back(res->d_members);
                component_list2(sub);
                d_scopes.pop_back();
                break;
            case Tok_end:
                break;
            }
        }
        return res;
    }

    void guid(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_guid);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            }
        }
    }

    Type::Ref class_reference_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_class_reference_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_name:
                res = type_name(sub);
                break;
            }
        }
        return res;
    }

    // TODO
    Type::Ref generic_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_generic_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_generic:
                break;
            case Tok_ident:
                break;
            case Tok_Lt:
                break;
            case SynTree::R_template_list:
                template_list(sub);
                break;
            case Tok_Gt:
                break;
            case Tok_Eq:
                break;
            case SynTree::R_generic_type_:
                generic_type_(sub);
                break;
            case Tok_Semi:
                break;
            }
        }
        return res;
    }

    void template_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_template_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list2:
                identifier_list2(sub);
                break;
            case Tok_Semi:
                break;
            }
        }
    }

    void identifier_list2(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_identifier_list2);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                identifier_list(sub);
                break;
            case Tok_Colon:
                break;
            case SynTree::R_type_name:
                type_name(sub);
                break;
            case Tok_Comma:
                break;
            case Tok_class:
                break;
            case Tok_interface:
                break;
            case Tok_object:
                break;
            case Tok_record:
                break;
            }
        }
    }

    void generic_type_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_generic_type_);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_class_type:
                class_type(sub);
                break;
            case SynTree::R_object_type:
                object_type(sub);
                break;
            case SynTree::R_interface_type:
                interface_type(sub);
                break;
            case SynTree::R_procedural_type:
                procedural_type(sub);
                break;
            case SynTree::R_record_type:
                record_type(sub);
                break;
            case SynTree::R_array_type:
                array_type(sub);
                break;
            }
        }
    }

    Type::Ref specialized_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_specialized_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_specialize:
                break;
            case Tok_ident:
                break;
            case Tok_Lt:
                break;
            case SynTree::R_type_name_list:
                type_name_list(sub);
                break;
            case Tok_Gt:
                break;
            }
        }
        return res;
    }

    void type_name_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_type_name_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_name:
                type_name(sub);
                break;
            case Tok_Comma:
                break;
            }
        }
    }

    Type::Ref helper_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_helper_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_class:
                break;
            case Tok_record:
                break;
            case Tok_type:
                break;
            case Tok_helper:
                break;
            case Tok_Lpar:
                break;
            case SynTree::R_base_helper:
                base_helper(sub);
                break;
            case Tok_Rpar:
                break;
            case Tok_for:
                break;
            case Tok_ident:
                break;
            case SynTree::R_helper_component_list:
                helper_component_list(sub);
                break;
            case Tok_end:
                break;
            case SynTree::R_hintdirectives:
                hintdirectives(sub);
                break;
            }
        }
        return res;
    }

    void helper_component_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_helper_component_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_method_definition:
                method_definition(sub);
                break;
            case SynTree::R_property_definition:
                property_definition(sub);
                break;
            }
        }
    }

    void expression(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_expression);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_simple_expression:
                simple_expression(sub);
                break;
            case SynTree::R_relop:
                relop(sub);
                break;
            }
        }
    }

    void relop(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_relop);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_Lt:
                break;
            case Tok_Leq:
                break;
            case Tok_Gt:
                break;
            case Tok_Geq:
                break;
            case Tok_Eq:
                break;
            case Tok_LtGt:
                break;
            case Tok_in:
                break;
            case Tok_is:
                break;
            }
        }
    }

    void simple_expression(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_simple_expression);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_term:
                term(sub);
                break;
            case SynTree::R_adop:
                adop(sub);
                break;
            }
        }
    }

    void simple_constant_expression(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_simple_constant_expression);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_simple_expression:
                simple_expression(sub);
                break;
            }
        }
    }

    void adop(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_adop);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_Plus:
                break;
            case Tok_Minus:
                break;
            case Tok_or:
                break;
            case Tok_xor:
                break;
            case Tok_GtLt:
                break;
            }
        }
    }

    void term(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_term);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_factor:
                factor(sub);
                break;
            case SynTree::R_mulop:
                mulop(sub);
                break;
            }
        }
    }

    void mulop(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_mulop);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_Star:
                break;
            case Tok_Slash:
                break;
            case Tok_div:
                break;
            case Tok_mod:
                break;
            case Tok_and:
                break;
            case Tok_shl:
                break;
            case Tok_shr:
                break;
            case Tok_as:
                break;
            case Tok_2Lt:
                break;
            case Tok_2Gt:
                break;
            }
        }
    }

    void factor(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_factor);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_nested_expr_or_structured_const:
                nested_expr_or_structured_const(sub);
                break;
            case SynTree::R_selector:
                selector(sub,0); // TODO
                break;
            case SynTree::R_varref_or_funcall_or_constid_or_cast:
                varref_or_funcall_or_constid_or_cast(sub);
                break;
            case SynTree::R_unsigned_constant:
                unsigned_constant(sub);
                break;
            case Tok_not:
                break;
            case SynTree::R_factor:
                factor(sub);
                break;
            case SynTree::R_sign:
                sign(sub);
                break;
            case SynTree::R_set_constructor:
                set_constructor(sub);
                break;
            case SynTree::R_address_factor:
                address_factor(sub);
                break;
            case SynTree::R_inherited_call:
                inherited_call(sub);
                break;
            }
        }
    }

    void varref_or_funcall_or_constid_or_cast(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_varref_or_funcall_or_constid_or_cast);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_designator:
                designator(sub);
                break;
            }
        }
    }

    void nested_expr_or_structured_const(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_nested_expr_or_structured_const);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_element:
                element(sub);
                break;
            }
        }
    }

    void element(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_element);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                // TODO
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void unsigned_constant(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_unsigned_constant);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_unsigned_number:
                unsigned_number(sub);
                break;
            case SynTree::R_character_string:
                character_string(sub);
                break;
            case Tok_nil:
                break;
            }
        }
    }

    void sign(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_sign);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_Plus:
                break;
            case Tok_Minus:
                break;
            }
        }
    }

    void actual_parameter_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_actual_parameter_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void set_constructor(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_set_constructor);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_set_group:
                set_group(sub);
                break;
            }
        }
    }

    void set_group(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_set_group);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            case Tok_2Dot:
                break;
            }
        }
    }

    void address_factor(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_address_factor);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_designator:
                designator(sub);
                break;
            case SynTree::R_nested_expr_or_structured_const:
                nested_expr_or_structured_const(sub);
                break;
            }
        }
    }

    void constant_expression(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_constant_expression);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_label_def:
                label_def(sub);
                break;
            case SynTree::R_simple_statement:
                simple_statement(sub);
                break;
            case SynTree::R_structured_statement:
                structured_statement(sub);
                break;
            case SynTree::R_asm_statement:
                asm_statement(sub);
                break;
            }
        }
    }

    void simple_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_simple_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_inherited_call:
                inherited_call(sub);
                break;
            case SynTree::R_assig_or_call:
                assig_or_call(sub);
                break;
            case SynTree::R_goto_statement:
                goto_statement(sub);
                break;
            case SynTree::R_raise_statement:
                raise_statement(sub);
                break;
            }
        }
    }

    void assig_or_call(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_assig_or_call);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_designator:
                designator(sub);
                break;
            case SynTree::R_nested_expr_or_structured_const:
                nested_expr_or_structured_const(sub);
                break;
            case SynTree::R_selector:
                selector(sub, 0); // TODO
                break;
            case SynTree::R_assigop:
                assigop(sub);
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void inherited_call(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_inherited_call);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                // TODO
                break;
            case SynTree::R_actual_parameter_list:
                actual_parameter_list(sub);
                break;
            }
        }
    }

    void assigop(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_assigop);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_ColonEq:
                break;
            case Tok_PlusEq:
                break;
            case Tok_MinusEq:
                break;
            case Tok_StarEq:
                break;
            case Tok_SlashEq:
                break;
            }
        }
    }

    void goto_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_goto_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_label_def:
                label_def(sub);
                break;
            }
        }
    }

    void structured_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_structured_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_compound_statement:
                compound_statement(sub);
                break;
            case SynTree::R_conditional_statement:
                conditional_statement(sub);
                break;
            case SynTree::R_repetitive_statement:
                repetitive_statement(sub);
                break;
            case SynTree::R_with_statement:
                with_statement(sub);
                break;
            case SynTree::R_try_statement:
                try_statement(sub);
                break;
            }
        }
    }

    void conditional_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_conditional_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_case_statement:
                case_statement(sub);
                break;
            case SynTree::R_if_statement:
                if_statement(sub);
                break;
            }
        }
    }

    void repetitive_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_repetitive_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_for_statement:
                for_statement(sub);
                break;
            case SynTree::R_repeat_statement:
                repeat_statement(sub);
                break;
            case SynTree::R_while_statement:
                while_statement(sub);
                break;
            }
        }
    }

    void compound_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_compound_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement_list:
                statement_list(sub);
                break;
            }
        }
    }

    void statement_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_statement_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement:
                statement(sub);
                break;
            }
        }
    }

    void case_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_case_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            case SynTree::R_case_part:
                case_part(sub);
                break;
            case SynTree::R_else_part:
                else_part(sub);
                break;
            }
        }
    }

    void case_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_case_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_case_range:
                case_range(sub);
                break;
            case SynTree::R_statement:
                statement(sub);
                break;
            }
        }
    }

    void case_range(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_case_range);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_constant_expression:
                constant_expression(sub);
                break;
            }
        }
    }

    void else_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_else_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement_list:
                statement_list(sub);
                break;
            }
        }
    }

    void if_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_if_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            case SynTree::R_statement:
                statement(sub);
                break;
            }
        }
    }

    void for_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_for_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_control_variable:
                control_variable(sub);
                break;
            case SynTree::R_initial_value:
                initial_value(sub);
                break;
            case SynTree::R_final_value:
                final_value(sub);
                break;
            case SynTree::R_enumerable:
                enumerable(sub);
                break;
            case SynTree::R_statement:
                statement(sub);
                break;
            }
        }
    }

    void control_variable(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_control_variable);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_variable_identifier:
                variable_identifier(sub);
                break;
            }
        }
    }

    void initial_value(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_initial_value);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void final_value(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_final_value);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void enumerable(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_enumerable);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_enumerated_type:
                enumerated_type(sub);
                break;
            case SynTree::R_designator:
                designator(sub);
                break;
            case SynTree::R_set_constructor:
                set_constructor(sub);
                break;
            }
        }
    }

    void repeat_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_repeat_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement_list:
                statement_list(sub);
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void while_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_while_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            case SynTree::R_statement:
                statement(sub);
                break;
            }
        }
    }

    void with_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_with_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_variable_reference:
                variable_reference(sub);
                break;
            case SynTree::R_statement:
                statement(sub);
                break;
            }
        }
    }

    void asm_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_asm_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_register_list:
                register_list(sub);
                break;
            }
        }
    }

    void register_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_register_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            }
        }
    }

    void procedure_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_procedure_declaration);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_procedure_header:
                d = function_procedure_header(sub, Thing::Proc, true);
                break;
            case SynTree::R_subroutine_block:
                d_scopes.push_back(d->d_body);
                subroutine_block(sub);
                d_scopes.pop_back();
                break;
            }
        }
    }

    void subroutine_block(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_subroutine_block);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_block:
                block(sub);
                break;
            case SynTree::R_external_directive:
                external_directive(sub);
                break;
            case SynTree::R_asm_block:
                asm_block(sub);
                break;
            case Tok_forward:
                break;
            }
        }
    }

    void function_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_function_declaration);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_function_header:
                d = function_procedure_header(sub, Thing::Func, true);
                break;
            case SynTree::R_subroutine_block:
                d_scopes.push_back(d->d_body);
                subroutine_block(sub);
                d_scopes.pop_back();
                break;
            }
        }
    }

    Declaration* function_procedure_header(SynTree* st, int kind, bool impl = false) {
        Q_ASSERT(st && (st->d_tok.d_type == SynTree::R_function_header || st->d_tok.d_type == SynTree::R_procedure_header));
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_qualifier:
                d = createProcDecl(qualifier(sub), kind, impl);
                break;
            case SynTree::R_formal_parameter_list:
                d_scopes.push_back(d->d_body);
                formal_parameter_list(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_result_type:
                {
                    Type::Ref t = result_type(sub);
                    if( d )
                    {
                        d->d_type = t;
                        if( impl && kind == Thing::Func )
                        {
                            Token tmp;
                            tmp.d_val = "result";
                            tmp.d_lineNr = d->d_loc.d_pos.d_row;
                            tmp.d_colNr = d->d_loc.d_pos.d_col;
                            tmp.d_sourcePath = d->d_loc.d_filePath;
                            tmp.d_id = Token::toId(tmp.d_val);
                            Declaration* result = addDecl(d->d_body,tmp,Thing::Var);
                            result->d_type = t;
                        }
                    }
                }
                break;
            case SynTree::R_modifiers:
                modifiers(sub);
                break;
            case SynTree::R_hintdirectives:
                hintdirectives(sub);
                break;
            }
        }
        return d;
    }

    void formal_parameter_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_formal_parameter_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_parameter_declaration:
                parameter_declaration(sub);
                break;
            }
        }
    }

    void parameter_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_parameter_declaration);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_constant_parameter:
                constant_parameter(sub);
                break;
            case SynTree::R_value_parameter:
                value_parameter(sub);
                break;
            case SynTree::R_variable_parameter:
                variable_parameter(sub);
                break;
            case SynTree::R_out_parameter:
                out_parameter(sub);
                break;
            }
        }
    }

    void value_parameter(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_value_parameter);
        TokenList ids;
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_parameter_type:
                t = parameter_type(sub);
                break;
            case SynTree::R_default_parameter_value:
                default_parameter_value(sub);
                break;
            }
        }
        foreach( const Token& id, ids )
        {
            Declaration* d = addDecl(id, Thing::Param);
            d->d_type = t;
        }
    }

    void variable_parameter(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variable_parameter);
        TokenList ids;
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_parameter_type:
                t = parameter_type(sub);
                break;
            }
        }
        foreach( const Token& id, ids )
        {
            Declaration* d = addDecl(id, Thing::Param);
            d->d_type = t;
        }
    }

    void out_parameter(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_out_parameter);
        TokenList ids;
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_parameter_type:
                t = parameter_type(sub);
                break;
            }
        }
        foreach( const Token& id, ids )
        {
            Declaration* d = addDecl(id, Thing::Param);
            d->d_type = t;
        }
    }

    void constant_parameter(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_constant_parameter);
        TokenList ids;
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_identifier_list:
                ids = identifier_list(sub);
                break;
            case SynTree::R_parameter_type:
                t = parameter_type(sub);
                break;
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
        foreach( const Token& id, ids )
        {
            Declaration* d = addDecl(id, Thing::Param);
            d->d_type = t;
        }
    }

    Type::Ref parameter_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_parameter_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_name:
                res = type_name(sub);
                break;
            }
        }
        return res;
    }

    void external_directive(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_external_directive);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            case SynTree::R_integer_constant:
                integer_constant(sub);
                break;
            }
        }
    }

    void asm_block(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_asm_block);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_declaration_part:
                declaration_part(sub);
                break;
            }
        }
    }

    void modifiers(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_modifiers);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_modifier:
                modifier(sub);
                break;
            }
        }
    }

    void modifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_modifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            // NOP
            switch(sub->d_tok.d_type) {
            case Tok_export:
                break;
            case Tok_alias:
                break;
            case Tok_Colon:
                break;
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            case Tok_interrupt:
                break;
            case Tok_noreturn:
                break;
            case Tok_iocheck:
                break;
            case Tok_inline:
                break;
            case Tok_cdecl:
                break;
            case Tok_cppdecl:
                break;
            case Tok_hardfloat:
                break;
            case Tok_local:
                break;
            case Tok_mwpascal:
                break;
            case Tok_ms_abi_default:
                break;
            case Tok_ms_abi_cdecl:
                break;
            case Tok_nostackframe:
                break;
            case Tok_overload:
                break;
            case Tok_pascal:
                break;
            case Tok_register:
                break;
            case Tok_safecall:
                break;
            case Tok_saveregisters:
                break;
            case Tok_softfloat:
                break;
            case Tok_stdcall:
                break;
            case Tok_sysv_abi_default:
                break;
            case Tok_sysv_abi_cdecl:
                break;
            case Tok_vectorcall:
                break;
            case Tok_varargs:
                break;
            }
        }
    }

    Declaration* operator_header(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_operator_header);
        Declaration* d = new Declaration();
        d->d_kind = Thing::Proc;
        d->d_owner = d_scopes.back();
        d->d_body = new Scope();
        d->d_body->d_kind = Thing::Body;
        d->d_body->d_owner = d;
        d->d_body->d_outer = d_scopes.back();
        d_scopes.back()->d_order.append(d);
        d_scopes.push_back(d->d_body);
        Declaration* var = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_assignment_operator_definition:
                assignment_operator_definition(sub, d);
                break;
            case SynTree::R_arithmetic_operator_definition:
                arithmetic_operator_definition(sub, d);
                break;
            case SynTree::R_comparison_operator_definition:
                comparison_operator_definition(sub, d);
                break;
            case SynTree::R_logical_operator_definition:
                logical_operator_definition(sub, d);
                break;
            case SynTree::R_other_operator_definition:
                other_operator_definition(sub, d);
                break;
            case SynTree::R_result_identifier:
                {
                    Token id = result_identifier(sub);
                    var = addDecl(id, Thing::Var);
                }
                break;
            case SynTree::R_result_type:
                d->d_type = result_type(sub);
                d->d_kind = Thing::Func;
                if( var )
                    var->d_type = d->d_type;
                break;
            }
        }
        d_scopes.pop_back();
        return d;
    }

    void operator_definition(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_operator_definition);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_operator_header:
                d = operator_header(sub);
                break;
            case SynTree::R_subroutine_block:
                d_scopes.push_back(d->d_body);
                subroutine_block(sub);
                d_scopes.pop_back();
                break;
            }
        }
    }

    void assignment_operator_definition(SynTree* st, Declaration* d) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_assignment_operator_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ColonEq:
            case Tok_explicit:
                d->d_name = sub->d_tok.d_val;
                d->d_loc.d_pos = sub->d_tok.toLoc();
                d->d_loc.d_filePath = sub->d_tok.d_sourcePath;
                d->d_id = sub->d_tok.d_id;
                break;
            case SynTree::R_parameter_list:
                parameter_list(sub);
                break;
            }
        }
    }

    void arithmetic_operator_definition(SynTree* st, Declaration* d) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_arithmetic_operator_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Plus:
            case Tok_Minus:
            case Tok_Star:
            case Tok_Slash:
            case Tok_2Star:
            case Tok_GtLt:
            case Tok_div:
            case Tok_mod:
            case Tok_shl:
            case Tok_shr:
                d->d_name = sub->d_tok.d_val;
                d->d_loc.d_pos = sub->d_tok.toLoc();
                d->d_loc.d_filePath = sub->d_tok.d_sourcePath;
                d->d_id = sub->d_tok.d_id;
                break;
            case SynTree::R_parameter_list:
                parameter_list(sub);
                break;
            }
        }
    }

    void comparison_operator_definition(SynTree* st, Declaration* d) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_comparison_operator_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Eq:
            case Tok_Lt:
            case Tok_Leq:
            case Tok_Gt:
            case Tok_Geq:
            case Tok_LtGt:
            case Tok_in:
                d->d_name = sub->d_tok.d_val;
                d->d_loc.d_pos = sub->d_tok.toLoc();
                d->d_loc.d_filePath = sub->d_tok.d_sourcePath;
                d->d_id = sub->d_tok.d_id;
                break;
            case SynTree::R_parameter_list:
                parameter_list(sub);
                break;
            }
        }
    }

    void logical_operator_definition(SynTree* st, Declaration* d) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_logical_operator_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_and:
            case Tok_or:
            case Tok_xor:
            case Tok_not:
                d->d_name = sub->d_tok.d_val;
                d->d_loc.d_pos = sub->d_tok.toLoc();
                d->d_loc.d_filePath = sub->d_tok.d_sourcePath;
                d->d_id = sub->d_tok.d_id;
                break;
            case SynTree::R_parameter_list:
                parameter_list(sub);
                break;
            }
        }
    }

    void other_operator_definition(SynTree* st, Declaration* d) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_other_operator_definition);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_enumerator:
            case Tok_inc:
            case Tok_dec:
                d->d_name = sub->d_tok.d_val;
                d->d_loc.d_pos = sub->d_tok.toLoc();
                d->d_loc.d_filePath = sub->d_tok.d_sourcePath;
                d->d_id = sub->d_tok.d_id;
                break;
            case SynTree::R_parameter_list:
                parameter_list(sub);
                break;
            }
        }
    }

    void raise_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_raise_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_exception_instance:
                exception_instance(sub);
                break;
            case SynTree::R_exception_address:
                exception_address(sub);
                break;
            }
        }
    }

    void exception_address(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_exception_address);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_address_expression:
                address_expression(sub);
                break;
            }
        }
    }

    void try_statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_try_statement);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement_list:
                statement_list(sub);
                break;
            case SynTree::R_exceptionhandlers:
                exceptionhandlers(sub);
                break;
            }
        }
    }

    void exceptionhandlers(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_exceptionhandlers);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_statement_list:
                statement_list(sub);
                break;
            case SynTree::R_exception_handler:
                exception_handler(sub);
                break;
            }
        }
    }

    void exception_handler(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_exception_handler);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                // TODO: if there is an ident this is in fact a local var declaration which requires a local scope!
                break;
            case SynTree::R_type_name:
                type_name(sub);
                break;
            case SynTree::R_statement:
                statement(sub);
                break;
            }
        }
    }

    void string_literal(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_string_literal);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            }
        }
    }

    void string_constant(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_string_constant);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_character_string:
                character_string(sub);
                break;
            }
        }
    }

    void integer_constant(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_integer_constant);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_unsigned_integer:
                unsigned_integer(sub);
                break;
            }
        }
    }

    Type::Ref result_type(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_result_type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_type_:
                res = type_(sub);
                break;
            }
        }
        return res;
    }

    void property_declaration_part(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_property_declaration_part);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_property_definition:
                property_definition(sub);
                break;
            }
        }
    }

    void exception_instance(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_exception_instance);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_designator:
                designator(sub);
                break;
            }
        }
    }

    Token result_identifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_result_identifier);
        Token id;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                id = sub->d_tok;
                break;
            }
        }
        return id;
    }

    void variable_reference(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variable_reference);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_designator:
                designator(sub);
                break;
            }
        }
    }

    void variable_identifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_variable_identifier);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addSym(d_scopes.back(), sub->d_tok);
                break;
            }
        }
    }

    void parameter_list(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_parameter_list);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_parameter_declaration:
                parameter_declaration(sub);
                break;
            }
        }
    }

    void default_parameter_value(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_default_parameter_value);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void constant(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_constant);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_unsigned_number:
                unsigned_number(sub);
                break;
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            case Tok_ident:
                addSym(d_scopes.back(), sub->d_tok);
                break;
            }
        }
    }

    void base_helper(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_base_helper);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addSym(d_scopes.back(), sub->d_tok);
               break;
            }
        }
    }

    void string_constant_declaration(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_string_constant_declaration);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                addDecl( sub->d_tok, Thing::Const);
                break;
            case SynTree::R_string_constant:
                string_constant(sub);
                break;
            }
        }
    }

    void address_expression(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_address_expression);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_expression:
                expression(sub);
                break;
            }
        }
    }

    void subrange(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_subrange);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_unsigned_integer:
                unsigned_integer(sub);
                break;
            }
        }
    }

    void label_def(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_label_def);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_decimal_int:
                break;
            case Tok_ident:
                addDecl(sub->d_tok,Thing::Label);
                break;
            }
        }
    }

    void unsigned_number(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_unsigned_number);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_unsigned_integer:
                unsigned_integer(sub);
                break;
            case Tok_unsigned_real:
                break;
            }
        }
    }

    void unsigned_integer(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_unsigned_integer);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_decimal_int:
                break;
            case Tok_hex_int:
                break;
            case Tok_octal_int:
                break;
            case Tok_binary_int:
                break;
            }
        }
    }

    void character_string(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_character_string);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_quoted_string:
                break;
            case SynTree::R_control_string:
                control_string(sub);
                break;
            }
        }
    }

    void control_string(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_control_string);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Hash:
                break;
            case SynTree::R_unsigned_integer:
                unsigned_integer(sub);
                break;
            }
        }
    }

    void designator(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_designator);
        Type* t = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                {
                    Symbol* sym = addSym(d_scopes.back(),sub->d_tok);
                    if( sym && sym->d_decl && sym->d_decl->isDeclaration() )
                        t = static_cast<Declaration*>(sym->d_decl)->d_type.data();
                }
                break;
            case Tok_string:
                break;
            case SynTree::R_selector:
                selector(sub, t);
                break;
            case SynTree::R_subrange:
                subrange(sub);
                break;
            }
        }
    }

    Type* selector(SynTree* st, Type* t) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_selector);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                if( t && t->d_members )
                {
                    Symbol* sym = addSym(t->d_members,sub->d_tok);
                    if( sym && sym->d_decl && sym->d_decl->isDeclaration() )
                        t = static_cast<Declaration*>(sym->d_decl)->d_type.data();
                }
                break;
            case SynTree::R_expression:
                // TODO
                expression(sub);
                break;
            case Tok_Hat:
                // TODO
                break;
            case SynTree::R_actual_parameter_list:
                // TODO
                actual_parameter_list(sub);
                break;
            }
        }
        return t;
    }

    TokenList qualifier(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_qualifier);
        TokenList res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                res.append(sub->d_tok);
                break;
            case Tok_Dot:
                break;
            }
        }
        return res;
    }

    Declaration* addDecl(const Token& t, int type )
    {
        return addDecl( d_scopes.back(), t, type );
    }

    Declaration* addDecl(Scope* scope, const Token& t, int type )
    {
        Declaration* d = new Declaration();
        d->d_kind = type;
        d->d_name = t.d_val;
        d->d_id = t.d_id;
        if( type != Thing::MethBlock )
        {
            d->d_loc.d_pos = t.toLoc();
            d->d_loc.d_filePath = t.d_sourcePath;
        }
        d->d_owner = scope;
        scope->d_order.append(d);

        const bool isFuncProc = type == Thing::Proc || type == Thing::Func;

        if( isFuncProc && d_cf->d_intf && scope == d_cf->d_intf )
            d_forwards.append(d);

        if( type == Thing::MethBlock )
            return d;

        // each decl is also a symbol

        Declaration* fwd = 0;

        Symbol* sy = new Symbol();
        sy->d_loc = t.toLoc();
        d_cf->d_syms[t.d_sourcePath].append(sy);
        d->d_me = sy;

        if( isFuncProc && scope == d_cf->d_impl && ( fwd = findInForwards(t) ) )
        {
            // add the present symbol which points to the interface twin
            sy->d_decl = fwd;
            fwd->d_refs[t.d_sourcePath].append(sy);

            d_redirect[d] = fwd;
            fwd->d_impl = d;
        }else if( isFuncProc && scope->d_kind == Thing::Members && ( fwd = findInMembers(scope->d_outer,t) ) )
        {
            sy->d_decl = fwd;
            fwd->d_refs[t.d_sourcePath].append(sy);
            fwd->d_impl = d;
        }else
        {
            sy->d_decl = d;
            d->d_refs[t.d_sourcePath].append(sy);
        }

        return d;
    }

    Declaration* findInForwards(const Token&t)
    {
        Declaration* res = 0;
        foreach( Declaration* d, d_forwards )
        {
            if( d->d_id == t.d_id )
            {
                res = d;
                break;
            }
        }
        return res;
    }

    Declaration* findInMembers(Scope* scope, const Token&t)
    {
        Declaration* res = 0;
        // if this is in a class declaration search there
        foreach( Declaration* d, scope->d_order )
        {
            if( d->d_id == t.d_id )
            {
                res = d;
                break;
            }
        }
        return res;
    }

    Symbol* addSym(Scope* scope, const Token& t)
    {
        Declaration* d = scope->findDecl(t.d_id);
#ifndef LISA_WITH_MISSING
        if( d )
#else
        if( true )
#endif
        {
            if( d && d_cf->d_kind == Thing::Unit )
            {
                QHash<Declaration*,Declaration*>::const_iterator intf = d_redirect.find(d);
                if( intf != d_redirect.end() )
                {
                    d = intf.value(); // attach all refs to the interface declaration (otherwise they are not visible)
                }
            }
            Symbol* sy = new Symbol();
            sy->d_loc = t.toLoc();
            d_cf->d_syms[t.d_sourcePath].append(sy);
            sy->d_decl = d;
            if( d )
                d->d_refs[t.d_sourcePath].append(sy);
            return sy;
        }else
            return 0;
    }

    Type::Ref resolvedType(Symbol* sym)
    {
        Type::Ref res;
        if( sym && sym->d_decl && sym->d_decl->d_kind == Thing::TypeDecl )
        {
            Declaration* d = static_cast<Declaration*>(sym->d_decl);
            res = d->d_type;
        }
        return res;
    }

    Symbol* type_identifier(Scope* scope, SynTree* st)
    {
        Symbol* res = 0;
        foreach( SynTree* s, st->d_children )
            if( s->d_tok.d_type == Tok_ident)
                res = addSym(scope,s->d_tok);
        return res;
    }

    Type* resolveMember( const TokenList& desig, Scope* scope )
    {
        Type* res = 0;
        if( scope == 0 )
            return 0;
        foreach( const Token& tok, desig )
        {
            Symbol* sym = addSym(scope,tok);
            if( sym && sym->d_decl && sym->d_decl->isDeclaration() )
                res = static_cast<Declaration*>(sym->d_decl)->d_type.data();
            else
                res = 0;
            if( res && res->d_members )
                scope = res->d_members;
        }
        return res;
    }

    Declaration* createProcDecl(const TokenList& id, int kind, bool impl = false)
    {
        Scope* scope = d_scopes.back();

        if( impl && id.size() > 1 )
        {
            Declaration* cls = 0;
            Declaration* meths = 0;
            cls = scope->findDecl(id.first().d_id);
            if( cls && cls->d_kind == Thing::MethBlock )
            {
                // Method block already exists
                meths = cls;
                addSym(meths->d_body->d_outer,id.first());
            }else if( cls && cls->d_type && cls->d_type->d_kind == Type::Class )
            {
                addSym(scope,id.first());

                // Create Method block
                meths = addDecl(scope, id.first() ,Thing::MethBlock);
                meths->d_body = new Scope();
                meths->d_body->d_kind = Thing::Members;
                meths->d_body->d_outer = cls->d_type->d_members;
                meths->d_body->d_altOuter = scope;

                Token tmp;
                tmp.d_val = "self";
                // intenionally no loc or path in tmp
                tmp.d_id = Token::toId(tmp.d_val);
                Declaration* self = addDecl(meths->d_body,tmp,Thing::Self);
                self->d_type = cls->d_type;
            }
            if( meths )
                scope = meths->d_body;
        }

        // func or proc decl
        Declaration* d = addDecl(scope, id.last(), kind);
        d->d_body = new Scope();
        d->d_body->d_kind = Thing::Body;
        d->d_body->d_owner = d;
        d->d_body->d_outer = scope;

        if( d->d_me && d->d_me->d_decl != d )
        {
            Q_ASSERT(d->d_me->d_decl->isDeclaration());
            Declaration* intf = static_cast<Declaration*>(d->d_me->d_decl);

            d->d_body->d_altOuter = intf->d_body;
        }
        return d;
    }

};

CodeModel::CodeModel(QObject *parent) : ItemModel(parent),d_sloc(0),d_errCount(0)
{
    d_fs = new FileSystem(this);
}

bool CodeModel::load(const QString& rootDir)
{
    beginResetModel();
    d_root = ModelItem();
    d_top.clear();
    d_globals.clear();
    d_map1.clear();
    d_map2.clear();
    d_sloc = 0;
    d_errCount = 0;
    d_mutes.clear();
    d_fs->load(rootDir);
    QList<ModelItem*> fileSlots;
    fillFolders(&d_root,&d_fs->getRoot(), &d_top, fileSlots);
    foreach( ModelItem* s, fileSlots )
    {
        Q_ASSERT( s->d_thing );
        if( s->d_thing->d_kind != Thing::Unit )
            continue;
        UnitFile* f = static_cast<UnitFile*>(s->d_thing);
        Q_ASSERT( f->d_file );
        parseAndResolve(f);
        for( int i = 0; i < f->d_includes.size(); i++ )
        {
            if( f->d_includes[i]->d_file != 0 )
                new ModelItem(s, f->d_includes[i]);
        }
    }
    endResetModel();
    return true;
}

ItemModel::ItemModel(QObject* parent):QAbstractItemModel(parent)
{

}

const Thing* ItemModel::getThing(const QModelIndex& index) const
{
    if( !index.isValid() )
        return 0;
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    return s->d_thing;
}

QModelIndex ItemModel::findThing(const Thing* nt) const
{
    return findThing( &d_root, nt );
}

QVariant ItemModel::data(const QModelIndex& index, int role) const
{
    return QVariant();
}

Symbol*CodeModel::findSymbolBySourcePos(const QString& path, int line, int col) const
{
    UnitFile::SymList syms;

    UnitFile* uf = getUnitFile(path);
    if( uf != 0 )
        syms = uf->d_syms.value(path);


    // TODO maybe make that faster by ordering by rowcol and e.g. binary search
    foreach( Symbol* s, syms )
    {
        if( s->d_decl && s->d_loc.d_row == line &&
                s->d_loc.d_col <= col && col < s->d_loc.d_col + s->d_decl->getLen() )
            return s;
    }
    return 0;
}

CodeFile*CodeModel::getCodeFile(const QString& path) const
{
    return d_map2.value(path);
}

UnitFile*CodeModel::getUnitFile(const QString& path) const
{
    CodeFile* cf = d_map2.value(path);;
    if( cf )
    {
        UnitFile* uf = cf->toUnit();
        if( uf == 0 )
        {
            IncludeFile* inc = cf->toInclude();
            if( inc )
                uf = inc->d_unit;
        }
        return uf;
    }
    return 0;
}

Ranges CodeModel::getMutes(const QString& path)
{
    return d_mutes.value(path);
}

QVariant CodeModel::data(const QModelIndex& index, int role) const
{
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    switch( role )
    {
    case Qt::DisplayRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            return static_cast<UnitFile*>(s->d_thing)->d_file->d_name;
        case Thing::Include:
            return static_cast<IncludeFile*>(s->d_thing)->d_file->d_name;
        case Thing::Folder:
            {
                CodeFolder* cf = static_cast<CodeFolder*>(s->d_thing);
                if( cf->d_dir->d_name.isEmpty() && ( s->d_parent == 0 || s->d_parent->d_parent == 0 ) )
                    return "<root>";
                return cf->d_dir->d_name;
            }
        }
        break;
    case Qt::DecorationRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            return QPixmap(":/images/source.png");
        case Thing::Include:
            return QPixmap(":/images/include.png");
        case Thing::Folder:
            return QPixmap(":/images/folder.png");
        }
        break;
    case Qt::ToolTipRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            {
                UnitFile* cf = static_cast<UnitFile*>(s->d_thing);
                return QString("<html><b>%1 %2</b><br>"
                               "<p>Logical path: %3</p>"
                               "<p>Real path: %4</p></html>")
                        .arg(cf->d_file->d_type == FileSystem::PascalLibrary ? "Library" : FileSystem::PascalUnit ? "Unit" : "Program")
                        .arg(cf->d_file->d_moduleName)
                        .arg(cf->d_file->getVirtualPath())
                        .arg(cf->d_file->d_realPath);
            }
        case Thing::Include:
            return static_cast<IncludeFile*>(s->d_thing)->d_file->d_realPath;
        }
        break;
    case Qt::FontRole:
        break;
    case Qt::ForegroundRole:
        break;
    }
    return QVariant();
}

QModelIndex ItemModel::index(int row, int column, const QModelIndex& parent) const
{
    const ModelItem* s = &d_root;
    if( parent.isValid() )
    {
        s = static_cast<ModelItem*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
    }
    if( row < s->d_children.size() && column < columnCount( parent ) )
        return createIndex( row, column, s->d_children[row] );
    else
        return QModelIndex();
}

QModelIndex ItemModel::parent(const QModelIndex& index) const
{
    if( index.isValid() )
    {
        ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
        Q_ASSERT( s != 0 );
        if( s->d_parent == &d_root )
            return QModelIndex();
        // else
        Q_ASSERT( s->d_parent != 0 );
        Q_ASSERT( s->d_parent->d_parent != 0 );
        return createIndex( s->d_parent->d_parent->d_children.indexOf( s->d_parent ), 0, s->d_parent );
    }else
        return QModelIndex();
}

int ItemModel::rowCount(const QModelIndex& parent) const
{
    if( parent.isValid() )
    {
        ModelItem* s = static_cast<ModelItem*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
        return s->d_children.size();
    }else
        return d_root.d_children.size();
}


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
    Lex( FileSystem* fs ):lex(fs) {}
};

void CodeModel::parseAndResolve(UnitFile* unit)
{
    if( unit->d_file->d_parsed )
        return; // already done

    QByteArrayList usedNames = unit->findUses();
    for( int i = 0; i < usedNames.size(); i++ )
    {
        const FileSystem::File* u = d_fs->findModule(unit->d_file->d_dir,usedNames[i].toLower());
        if( u == 0 )
        {
            const QString line = tr("%1: cannot resolve referenced unit '%2'")
                    .arg( unit->d_file->getVirtualPath(false) ).arg(usedNames[i].constData());
            qCritical() << line.toUtf8().constData();
            d_errCount++;
        }else
        {
            UnitFile* uf = d_map1.value(u);
            Q_ASSERT( uf );
            unit->d_import.append( uf );
            parseAndResolve(uf);
        }
    }

    const_cast<FileSystem::File*>(unit->d_file)->d_parsed = true;
    Lex lex(d_fs);
    lex.lex.reset(unit->d_file->d_realPath);

    // TODO: make adaptable
    const QString root = d_fs->getRootPath();
    lex.lex.setSearchPaths(QStringList() << root << root+"/sparc" << root+"/i386" );
    lex.lex.addMacro("cpu32bitalu");
    lex.lex.addMacro("i386");
    lex.lex.addMacro("GnuLd");

    Parser p(&lex);
    p.RunParser();
    const int off = d_fs->getRootPath().size();
    if( !p.errors.isEmpty() )
    {
        foreach( const Parser::Error& e, p.errors )
        {
            const FileSystem::File* f = d_fs->findFile(e.path);
            const QString line = tr("%1:%2:%3: %4").arg( f ? f->getVirtualPath() : e.path.mid(off) ).arg(e.row)
                    .arg(e.col).arg(e.msg);
            qCritical() << line.toUtf8().constData();
            d_errCount++;
        }

    }
    foreach( const PpLexer::Include& f, lex.lex.getIncludes() )
    {
        IncludeFile* inc = new IncludeFile();
        inc->d_file = d_fs->findFile(f.d_path);
        inc->d_len = f.d_len;
        inc->d_unit = unit;
        inc->d_folder = unit->d_folder;
        Symbol* sym = new Symbol();
        sym->d_decl = inc;
        sym->d_loc = f.d_loc;
        unit->d_syms[f.d_sourcePath].append(sym);
        if( inc->d_file )
            d_map2[inc->d_file->d_realPath] = inc;
        unit->d_includes.append(inc);
    }
    d_sloc += lex.lex.getSloc();
    for( QHash<QString,Ranges>::const_iterator i = lex.lex.getMutes().begin(); i != lex.lex.getMutes().end(); ++i )
        d_mutes.insert(i.key(),i.value());

    PascalModelVisitor v(this);
    v.visit(unit,&p.root); // visit takes ~8% more time than just parsing

    QCoreApplication::processEvents();
}

QModelIndex ItemModel::findThing(const ModelItem* slot, const Thing* nt) const
{
    for( int i = 0; i < slot->d_children.size(); i++ )
    {
        ModelItem* s = slot->d_children[i];
        if( s->d_thing == nt )
            return createIndex( i, 0, s );
        QModelIndex index = findThing( s, nt );
        if( index.isValid() )
            return index;
    }
    return QModelIndex();
}

bool ModelItem::lessThan(const ModelItem* lhs, const ModelItem* rhs)
{
    if( lhs->d_thing == 0 || rhs->d_thing == 0 )
        return false;
    return lhs->d_thing->getName().compare(rhs->d_thing->getName(),Qt::CaseInsensitive) < 0;
}

void CodeModel::fillFolders(ModelItem* root, const FileSystem::Dir* super, CodeFolder* top, QList<ModelItem*>& fileSlots)
{
    for( int i = 0; i < super->d_subdirs.size(); i++ )
    {
        CodeFolder* f = new CodeFolder();
        f->d_dir = super->d_subdirs[i];
        top->d_subs.append(f);
        ModelItem* s = new ModelItem(root,f);
        fillFolders(s,super->d_subdirs[i],f,fileSlots);
    }
    for( int i = 0; i < super->d_files.size(); i++ )
    {
        const int t = super->d_files[i]->d_type;
        if( t == FileSystem::PascalProgram ||
                t == FileSystem::PascalUnit ||
                t == FileSystem::PascalLibrary )
        {
            UnitFile* f = new UnitFile();
            f->d_file = super->d_files[i];
            f->d_folder = top;
            d_map1[f->d_file] = f;
            d_map2[f->d_file->d_realPath] = f;
            top->d_files.append(f);
            ModelItem* s = new ModelItem(root,f);
            fileSlots.append(s);
        }
    }
    std::sort( root->d_children.begin(), root->d_children.end(), ModelItem::lessThan );
}

QString CodeFile::getName() const
{
    return d_file->d_name;
}

QByteArrayList UnitFile::findUses() const
{
    QByteArrayList res;
    if( d_file == 0 || !(d_file->d_type == FileSystem::PascalProgram ||
                         d_file->d_type == FileSystem::PascalUnit ||
                         d_file->d_type == FileSystem::PascalLibrary ) )
        return res;
    QFile f(d_file->d_realPath);
    f.open(QIODevice::ReadOnly);
    Lexer lex;
    lex.setStream(&f);
    Token t = lex.nextToken();
    while( t.isValid() )
    {
        switch( t.d_type )
        {
        case Tok_uses:
            t = lex.nextToken();
            while( t.isValid() && t.d_type != Tok_Semi )
            {
                if( t.d_type == Tok_Comma )
                {
                    t = lex.nextToken();
                    continue;
                }
                if( t.d_type == Tok_ident )
                {
                    const QByteArray id = t.d_val;
                    t = lex.nextToken();
                    if( t.d_type == Tok_Slash )
                    {
                        t = lex.nextToken();
                        if( t.d_type == Tok_ident ) // just to make sure
                        {
                            res.append( t.d_val );
                            t = lex.nextToken();
                        }
                    }else
                        res.append(id);
                }else
                    t = lex.nextToken();
            }
            return res;
        case Tok_label:
        case Tok_var:
        case Tok_const:
        case Tok_type:
        case Tok_procedure:
        case Tok_function:
        case Tok_implementation:
            return res;
        }
        t = lex.nextToken();
    }
    return res;
}

UnitFile::~UnitFile()
{
    if( d_impl )
        delete d_impl;
    if( d_intf )
        delete d_intf;
    QHash<QString,SymList>::const_iterator j;
    for( j = d_syms.begin(); j != d_syms.end(); ++j )
        for( int i = 0; i < j.value().size(); i++ )
            delete j.value()[i];
    for( int i = 0; i < d_includes.size(); i++ )
        delete d_includes[i];
}

UnitFile*Scope::getUnitFile() const
{
    if( d_owner == 0 )
        return 0; // happens in global scope
    if( d_owner->d_kind == Thing::Unit )
        return static_cast<UnitFile*>(d_owner);
    else if( d_owner->isDeclaration() )
    {
        Declaration* d = static_cast<Declaration*>(d_owner);
        Q_ASSERT( d->d_owner != 0 );
        return d->d_owner->getUnitFile();
    }else
        return 0; // happens in assembler
}

Declaration*Scope::findDecl(const char* id, bool withImports) const
{
    foreach( Declaration* d, d_order )
    {
        if( d->d_id == id )
            return d;
    }
    if( d_altOuter )
    {
        foreach( Declaration* d, d_altOuter->d_order )
        {
            if( d->d_id == id )
                return d;
        }
    }
    if( d_outer )
        return d_outer->findDecl(id, withImports);

    UnitFile* cf = getUnitFile();
    if( cf == 0 )
        return 0; // TODO: this happens, check

    if( withImports )
    {
        // searching through imports takes ~12% more time
        foreach( UnitFile* imp, cf->d_import )
        {
            if( imp->d_intf )
            {
                Declaration* d = imp->d_intf->findDecl(id,false); // don't follow imports of imports
                if( d )
                    return d;
            }
        }
    }
    if( false ) // cf->d_globals )
        // not actually needed since uses resolution goes directly to globals
        return cf->d_globals->findDecl(id, false);
    return 0;
}

void Scope::clear()
{
    for( int i = 0; i < d_order.size(); i++ )
        delete d_order[i];
    d_order.clear();
}

Scope::~Scope()
{
    clear();
}

QString Declaration::getName() const
{
    return d_name;
}

UnitFile*Declaration::getUnitFile() const
{
    Q_ASSERT( d_owner != 0 );
    return d_owner->getUnitFile();
}

Declaration::~Declaration()
{
    if( d_body )
        delete d_body;
}

QString CodeFolder::getName() const
{
    return d_dir->d_name;
}

void CodeFolder::clear()
{
    for( int i = 0; i < d_subs.size(); i++ )
    {
        d_subs[i]->clear();
        delete d_subs[i];
    }
    d_subs.clear();
    for( int i = 0; i < d_files.size(); i++ )
        delete d_files[i];
    d_files.clear();
}

FilePos IncludeFile::getLoc() const
{
    if( d_file == 0 )
        return FilePos();
    return FilePos( RowCol(1,1), d_file->d_realPath );
}

QString Thing::getName() const
{
    return QString();
}

const char*Thing::typeName() const
{
    switch( d_kind )
    {
    case Const:
        return "Const";
    case TypeDecl:
        return "Type";
    case Var:
        return "Var";
    case Func:
        return "Function";
    case Proc:
        return "Procedure";
    case Module:
        return "Module";
    case Param:
        return "Parameter";
    case Field:
        return "Field";
    case Label:
        return "Label";
    default:
        return "";
    }
}

Thing::~Thing()
{
}

UnitFile*CodeFile::toUnit()
{
    if( d_kind == Unit )
        return static_cast<UnitFile*>(this);
    else
        return 0;
}

IncludeFile*CodeFile::toInclude()
{
    if( d_kind == Include )
        return static_cast<IncludeFile*>(this);
    else
        return 0;
}

CodeFile::~CodeFile()
{
}

Type::~Type()
{
    if( d_members )
        delete d_members;
}


ModuleDetailMdl::ModuleDetailMdl(QObject* parent)
{

}

void ModuleDetailMdl::load(Scope* intf, Scope* impl)
{
    if( d_intf == intf && d_impl == impl )
        return;

    beginResetModel();
    d_root = ModelItem();
    d_intf = intf;
    d_impl = impl;

    if( intf && impl )
    {
        ModelItem* title = new ModelItem( &d_root, intf );
        fillItems(title, intf);

        title = new ModelItem( &d_root, impl );
        fillItems(title, impl);

    }else if( impl )
        fillItems(&d_root, impl);

    endResetModel();
}

QVariant ModuleDetailMdl::data(const QModelIndex& index, int role) const
{
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    switch( role )
    {
    case Qt::DisplayRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Interface:
            return "interface";
        case Thing::Implementation:
            return "implementation";
        default:
            return s->d_thing->getName();
        }
        break;
    case Qt::DecorationRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Interface:
            return QPixmap(":/images/interface.png");
        case Thing::Implementation:
            return QPixmap(":/images/implementation.png");
        case Thing::Const:
            return QPixmap(":/images/constant.png");
        case Thing::TypeDecl:
            return QPixmap(":/images/type.png");
        case Thing::Var:
            return QPixmap(":/images/variable.png");
        case Thing::Func:
            return QPixmap(":/images/function.png");
        case Thing::Proc:
            return QPixmap(":/images/procedure.png");
        case Thing::Label:
            return QPixmap(":/images/label.png");
        case Thing::MethBlock:
            return QPixmap(":/images/category.png");
        }
        break;
    }
    return QVariant();
}

static bool DeclLessThan(Declaration* lhs, Declaration* rhs)
{
    return lhs->getName() < rhs->getName();
}

void ModuleDetailMdl::fillItems(ModelItem* parentItem, Scope* scope)
{
    QVector<QList<Declaration*> > elems(Thing::Label);
    foreach( Declaration* d, scope->d_order )
    {
        if( d->d_kind > Thing::Const && d->d_kind < Thing::Label )
            elems[ d->d_kind >= Thing::Proc ? d->d_kind-1 : d->d_kind ].append(d);
    }
    for( int i = 1; i < elems.size()-1; i++ ) // leave out consts and labels
    {
        QList<Declaration*>& d = elems[i];
        std::sort( d.begin(), d.end(), DeclLessThan );
        for( int j = 0; j < d.size(); j++ )
        {
            Declaration* dd = d[j];
            ModelItem* item = new ModelItem(parentItem,dd);
            if( dd->d_kind == Thing::TypeDecl && dd->d_type && dd->d_type->d_kind == Type::Class )
                fillSubs(item,dd->d_type->d_members);
            else if( dd->d_kind == Thing::MethBlock )
                fillSubs(item,dd->d_body);
        }
    }
}

void ModuleDetailMdl::fillSubs(ModelItem* parentItem, Scope* scope)
{
    QList<Declaration*> funcs;
    foreach( Declaration* d, scope->d_order )
    {
        if( ( d->d_kind == Thing::Func || d->d_kind == Thing::Proc ) )
            funcs.append(d);
    }
    std::sort( funcs.begin(), funcs.end(), DeclLessThan );
    for( int j = 0; j < funcs.size(); j++ )
        new ModelItem(parentItem,funcs[j]);
}

