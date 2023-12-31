#ifndef __FP_SYNTREE__
#define __FP_SYNTREE__
// This file was automatically generated by EbnfStudio; don't modify it!

#include <FreePascal/FpTokenType.h>
#include <FreePascal/FpToken.h>
#include <QList>

namespace Fp {

	struct SynTree {
		enum ParserRule {
			R_First = TT_Max + 1,
			R_FreePascal,
			R_actual_parameter_list,
			R_address_constant_,
			R_address_expression,
			R_address_factor,
			R_adop,
			R_arithmetic_operator_definition,
			R_array_type,
			R_asm_block,
			R_asm_statement,
			R_assig_or_call,
			R_assigned_enum_,
			R_assigned_enum_list,
			R_assignment_operator_definition,
			R_assignment_statement_,
			R_assigop,
			R_base_helper,
			R_block,
			R_call_modifiers,
			R_case_part,
			R_case_range,
			R_case_statement,
			R_character_string,
			R_class_part,
			R_class_reference_type,
			R_class_type,
			R_class_variable_declaration_part_,
			R_class_visibility_specifier,
			R_comment_,
			R_comparison_operator_definition,
			R_component_list,
			R_component_list2,
			R_component_list3,
			R_compound_statement,
			R_conditional_statement,
			R_const_definition,
			R_constant,
			R_constant_declaration,
			R_constant_declaration_part,
			R_constant_element_,
			R_constant_expression,
			R_constant_parameter,
			R_constructor_declaration,
			R_constructor_header,
			R_control_string,
			R_control_variable,
			R_declaration_part,
			R_default_parameter_value,
			R_default_specifier,
			R_defaultarraypropertyspecifier,
			R_designator,
			R_destructor_declaration,
			R_destructor_header,
			R_element,
			R_else_part,
			R_enumerable,
			R_enumerated_type,
			R_enumerated_type_,
			R_exception_address,
			R_exception_handler,
			R_exception_instance,
			R_exceptionhandlers,
			R_exports_clause,
			R_exports_entry,
			R_exports_list,
			R_expression,
			R_external_directive,
			R_factor,
			R_field_definition,
			R_field_definition2,
			R_field_list,
			R_field_or_function,
			R_field_or_procedure,
			R_file_type,
			R_final_value,
			R_finalization_part,
			R_fixed_field_,
			R_fixed_fields,
			R_for_in_statement_,
			R_for_statement,
			R_formal_parameter_list,
			R_func_proc_declaration_part,
			R_func_proc_header,
			R_function_call_,
			R_function_declaration,
			R_function_header,
			R_function_identifier_,
			R_generic_type,
			R_generic_type_,
			R_goto_statement,
			R_guid,
			R_helper_component_list,
			R_helper_type,
			R_heritage,
			R_heritage2,
			R_hint_directive,
			R_hintdirectives,
			R_identifier_list,
			R_identifier_list2,
			R_if_statement,
			R_implementation_part,
			R_implemented_interfaces,
			R_implements_specifier,
			R_inherited_call,
			R_initial_value,
			R_initialization_part,
			R_integer_constant,
			R_interface_part,
			R_interface_type,
			R_label_declaration_part,
			R_label_def,
			R_library_,
			R_library_header,
			R_logical_operator_definition,
			R_method_definition,
			R_method_definition2,
			R_method_designator_,
			R_method_directives,
			R_method_directives2,
			R_modifier,
			R_modifiers,
			R_mulop,
			R_nested_expr_or_structured_const,
			R_object_type,
			R_object_visibility_specifier,
			R_old_record_type_,
			R_operator_definition,
			R_operator_header,
			R_ordinal_type,
			R_other_operator_definition,
			R_out_parameter,
			R_packable_type_,
			R_parameter_declaration,
			R_parameter_list,
			R_parameter_type,
			R_pointer_type,
			R_procedural_type,
			R_procedure_declaration,
			R_procedure_header,
			R_procedure_headers_part,
			R_procedure_identifier_,
			R_procedure_statement_,
			R_program_,
			R_program_header,
			R_program_parameters,
			R_property_declaration_part,
			R_property_definition,
			R_property_definition2,
			R_property_interface,
			R_property_parameter_list,
			R_property_specifiers,
			R_property_specifiers2,
			R_pseudo_keywords_,
			R_qualified_method_identifier_,
			R_qualifier,
			R_raise_statement,
			R_read_specifier,
			R_record_method_definition,
			R_record_operator_definition,
			R_record_or_array_constant_,
			R_record_type,
			R_record_visibility_specifier,
			R_register_list,
			R_relop,
			R_repeat_statement,
			R_repetitive_statement,
			R_resourcestring_declaration_part,
			R_result_identifier,
			R_result_type,
			R_selector,
			R_set_constructor,
			R_set_group,
			R_set_type,
			R_sign,
			R_simple_constant_expression,
			R_simple_expression,
			R_simple_statement,
			R_simple_type,
			R_specialized_type,
			R_statement,
			R_statement_list,
			R_statement_part,
			R_stored_specifier,
			R_string_constant,
			R_string_constant_declaration,
			R_string_literal,
			R_string_type,
			R_structured_statement,
			R_structured_type,
			R_subrange,
			R_subrange_type,
			R_subroutine_block,
			R_template_list,
			R_term,
			R_threadvariable_declaration_part,
			R_try_statement,
			R_type_,
			R_type_alias_,
			R_type_declaration,
			R_type_declaration_part,
			R_type_name,
			R_type_name_list,
			R_typed_constant,
			R_typed_constant_declaration_,
			R_unit_,
			R_unit_header,
			R_unsigned_constant,
			R_unsigned_integer,
			R_unsigned_number,
			R_uses_clause,
			R_uses_clause_,
			R_value_parameter,
			R_value_typecast_,
			R_variable_declaration,
			R_variable_declaration_part,
			R_variable_identifier,
			R_variable_modifier_,
			R_variable_modifiers,
			R_variable_parameter,
			R_variable_reference,
			R_variant,
			R_variant_part,
			R_varref_or_funcall_or_constid_or_cast,
			R_while_statement,
			R_with_statement,
			R_write_specifier,
			R_Last
		};
		SynTree(quint16 r = Tok_Invalid, const Token& = Token() );
		SynTree(const Token& t ):d_tok(t){}
		~SynTree() { foreach(SynTree* n, d_children) delete n; }

		static const char* rToStr( quint16 r );

		Fp::Token d_tok;
		QList<SynTree*> d_children;
	};

}
#endif // __FP_SYNTREE__
