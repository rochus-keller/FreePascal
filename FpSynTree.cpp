// This file was automatically generated by EbnfStudio; don't modify it!
#include "FpSynTree.h"
using namespace Fp;

SynTree::SynTree(quint16 r, const Token& t ):d_tok(r){
	d_tok.d_lineNr = t.d_lineNr;
	d_tok.d_colNr = t.d_colNr;
	d_tok.d_sourcePath = t.d_sourcePath;
}

const char* SynTree::rToStr( quint16 r ) {
	switch(r) {
		case R_FreePascal: return "FreePascal";
		case R_actual_parameter_list: return "actual_parameter_list";
		case R_address_constant_: return "address_constant";
		case R_address_expression: return "address_expression";
		case R_address_factor: return "address_factor";
		case R_adop: return "adop";
		case R_arithmetic_operator_definition: return "arithmetic_operator_definition";
		case R_array_type: return "array_type";
		case R_asm_block: return "asm_block";
		case R_asm_statement: return "asm_statement";
		case R_assig_or_call: return "assig_or_call";
		case R_assigned_enum_: return "assigned_enum_";
		case R_assigned_enum_list: return "assigned_enum_list";
		case R_assignment_operator_definition: return "assignment_operator_definition";
		case R_assignment_statement_: return "assignment_statement";
		case R_assigop: return "assigop";
		case R_base_helper: return "base_helper";
		case R_block: return "block";
		case R_call_modifiers: return "call_modifiers";
		case R_case_part: return "case_part";
		case R_case_range: return "case_range";
		case R_case_statement: return "case_statement";
		case R_character_string: return "character_string";
		case R_class_part: return "class_part";
		case R_class_reference_type: return "class_reference_type";
		case R_class_type: return "class_type";
		case R_class_variable_declaration_part_: return "class_variable_declaration_part";
		case R_class_visibility_specifier: return "class_visibility_specifier";
		case R_comment_: return "comment";
		case R_comparison_operator_definition: return "comparison_operator_definition";
		case R_component_list: return "component_list";
		case R_component_list2: return "component_list2";
		case R_component_list3: return "component_list3";
		case R_compound_statement: return "compound_statement";
		case R_conditional_statement: return "conditional_statement";
		case R_const_definition: return "const_definition";
		case R_constant: return "constant";
		case R_constant_declaration: return "constant_declaration";
		case R_constant_declaration_part: return "constant_declaration_part";
		case R_constant_element_: return "constant_element";
		case R_constant_expression: return "constant_expression";
		case R_constant_parameter: return "constant_parameter";
		case R_constructor_declaration: return "constructor_declaration";
		case R_constructor_header: return "constructor_header";
		case R_control_string: return "control_string";
		case R_control_variable: return "control_variable";
		case R_declaration_part: return "declaration_part";
		case R_default_parameter_value: return "default_parameter_value";
		case R_default_specifier: return "default_specifier";
		case R_defaultarraypropertyspecifier: return "defaultarraypropertyspecifier";
		case R_designator: return "designator";
		case R_destructor_declaration: return "destructor_declaration";
		case R_destructor_header: return "destructor_header";
		case R_element: return "element";
		case R_else_part: return "else_part";
		case R_enumerable: return "enumerable";
		case R_enumerated_type: return "enumerated_type";
		case R_enumerated_type_: return "enumerated_type_";
		case R_exception_address: return "exception_address";
		case R_exception_handler: return "exception_handler";
		case R_exception_instance: return "exception_instance";
		case R_exceptionhandlers: return "exceptionhandlers";
		case R_exports_clause: return "exports_clause";
		case R_exports_entry: return "exports_entry";
		case R_exports_list: return "exports_list";
		case R_expression: return "expression";
		case R_external_directive: return "external_directive";
		case R_factor: return "factor";
		case R_field_definition: return "field_definition";
		case R_field_definition2: return "field_definition2";
		case R_field_list: return "field_list";
		case R_field_or_function: return "field_or_function";
		case R_field_or_procedure: return "field_or_procedure";
		case R_file_type: return "file_type";
		case R_final_value: return "final_value";
		case R_finalization_part: return "finalization_part";
		case R_fixed_field_: return "fixed_field_";
		case R_fixed_fields: return "fixed_fields";
		case R_for_in_statement_: return "for_in_statement";
		case R_for_statement: return "for_statement";
		case R_formal_parameter_list: return "formal_parameter_list";
		case R_func_proc_declaration_part: return "func_proc_declaration_part";
		case R_func_proc_header: return "func_proc_header";
		case R_function_call_: return "function_call";
		case R_function_declaration: return "function_declaration";
		case R_function_header: return "function_header";
		case R_function_identifier_: return "function_identifier";
		case R_generic_type: return "generic_type";
		case R_generic_type_: return "generic_type_";
		case R_goto_statement: return "goto_statement";
		case R_guid: return "guid";
		case R_helper_component_list: return "helper_component_list";
		case R_helper_type: return "helper_type";
		case R_heritage: return "heritage";
		case R_heritage2: return "heritage2";
		case R_hint_directive: return "hint_directive";
		case R_hintdirectives: return "hintdirectives";
		case R_identifier: return "identifier";
		case R_identifier_list: return "identifier_list";
		case R_identifier_list2: return "identifier_list2";
		case R_if_statement: return "if_statement";
		case R_implementation_part: return "implementation_part";
		case R_implemented_interfaces: return "implemented_interfaces";
		case R_implements_specifier: return "implements_specifier";
		case R_inherited_call: return "inherited_call";
		case R_initial_value: return "initial_value";
		case R_initialization_part: return "initialization_part";
		case R_integer_constant: return "integer_constant";
		case R_interface_part: return "interface_part";
		case R_interface_type: return "interface_type";
		case R_label_declaration_part: return "label_declaration_part";
		case R_label_def: return "label_def";
		case R_library_: return "library_";
		case R_library_header: return "library_header";
		case R_logical_operator_definition: return "logical_operator_definition";
		case R_method_definition: return "method_definition";
		case R_method_definition2: return "method_definition2";
		case R_method_designator_: return "method_designator";
		case R_method_directives: return "method_directives";
		case R_method_directives2: return "method_directives2";
		case R_modifier: return "modifier";
		case R_modifiers: return "modifiers";
		case R_mulop: return "mulop";
		case R_nested_expr_or_structured_const: return "nested_expr_or_structured_const";
		case R_object_type: return "object_type";
		case R_object_visibility_specifier: return "object_visibility_specifier";
		case R_old_record_type_: return "old_record_type";
		case R_operator_definition: return "operator_definition";
		case R_operator_header: return "operator_header";
		case R_ordinal_type: return "ordinal_type";
		case R_other_operator_definition: return "other_operator_definition";
		case R_out_parameter: return "out_parameter";
		case R_packable_type_: return "packable_type_";
		case R_parameter_declaration: return "parameter_declaration";
		case R_parameter_list: return "parameter_list";
		case R_parameter_type: return "parameter_type";
		case R_pointer_type: return "pointer_type";
		case R_procedural_type: return "procedural_type";
		case R_procedure_declaration: return "procedure_declaration";
		case R_procedure_header: return "procedure_header";
		case R_procedure_headers_part: return "procedure_headers_part";
		case R_procedure_identifier_: return "procedure_identifier";
		case R_procedure_statement_: return "procedure_statement";
		case R_program_: return "program_";
		case R_program_header: return "program_header";
		case R_program_parameters: return "program_parameters";
		case R_property_declaration_part: return "property_declaration_part";
		case R_property_definition: return "property_definition";
		case R_property_definition2: return "property_definition2";
		case R_property_interface: return "property_interface";
		case R_property_parameter_list: return "property_parameter_list";
		case R_property_specifiers: return "property_specifiers";
		case R_property_specifiers2: return "property_specifiers2";
		case R_pseudo_keywords_: return "pseudo_keywords";
		case R_qualified_method_identifier_: return "qualified_method_identifier";
		case R_qualifier: return "qualifier";
		case R_raise_statement: return "raise_statement";
		case R_read_specifier: return "read_specifier";
		case R_record_method_definition: return "record_method_definition";
		case R_record_operator_definition: return "record_operator_definition";
		case R_record_or_array_constant_: return "record_or_array_constant";
		case R_record_type: return "record_type";
		case R_record_visibility_specifier: return "record_visibility_specifier";
		case R_register_list: return "register_list";
		case R_relop: return "relop";
		case R_repeat_statement: return "repeat_statement";
		case R_repetitive_statement: return "repetitive_statement";
		case R_resourcestring_declaration_part: return "resourcestring_declaration_part";
		case R_result_identifier: return "result_identifier";
		case R_result_type: return "result_type";
		case R_selector: return "selector";
		case R_set_constructor: return "set_constructor";
		case R_set_group: return "set_group";
		case R_set_type: return "set_type";
		case R_sign: return "sign";
		case R_simple_constant_expression: return "simple_constant_expression";
		case R_simple_expression: return "simple_expression";
		case R_simple_statement: return "simple_statement";
		case R_simple_type: return "simple_type";
		case R_specialized_type: return "specialized_type";
		case R_statement: return "statement";
		case R_statement_list: return "statement_list";
		case R_statement_part: return "statement_part";
		case R_stored_specifier: return "stored_specifier";
		case R_string_constant: return "string_constant";
		case R_string_constant_declaration: return "string_constant_declaration";
		case R_string_literal: return "string_literal";
		case R_string_type: return "string_type";
		case R_structured_statement: return "structured_statement";
		case R_structured_type: return "structured_type";
		case R_subrange: return "subrange";
		case R_subrange_type: return "subrange_type";
		case R_subroutine_block: return "subroutine_block";
		case R_template_list: return "template_list";
		case R_term: return "term";
		case R_threadvariable_declaration_part: return "threadvariable_declaration_part";
		case R_try_statement: return "try_statement";
		case R_type_: return "type_";
		case R_type_alias_: return "type_alias";
		case R_type_declaration: return "type_declaration";
		case R_type_declaration_part: return "type_declaration_part";
		case R_type_name: return "type_name";
		case R_type_name_list: return "type_name_list";
		case R_typed_constant: return "typed_constant";
		case R_typed_constant_declaration_: return "typed_constant_declaration";
		case R_unit_: return "unit_";
		case R_unit_header: return "unit_header";
		case R_unsigned_constant: return "unsigned_constant";
		case R_unsigned_integer: return "unsigned_integer";
		case R_unsigned_number: return "unsigned_number";
		case R_uses_clause: return "uses_clause";
		case R_uses_clause_: return "uses_clause_";
		case R_value_parameter: return "value_parameter";
		case R_value_typecast_: return "value_typecast";
		case R_variable_declaration: return "variable_declaration";
		case R_variable_declaration_part: return "variable_declaration_part";
		case R_variable_identifier: return "variable_identifier";
		case R_variable_modifier_: return "variable_modifier_";
		case R_variable_modifiers: return "variable_modifiers";
		case R_variable_parameter: return "variable_parameter";
		case R_variable_reference: return "variable_reference";
		case R_variant: return "variant";
		case R_variant_part: return "variant_part";
		case R_varref_or_funcall_or_constid_or_cast: return "varref_or_funcall_or_constid_or_cast";
		case R_while_statement: return "while_statement";
		case R_with_statement: return "with_statement";
		case R_write_specifier: return "write_specifier";
	default: if(r<R_First) return tokenTypeName(r); else return "";
}
}
