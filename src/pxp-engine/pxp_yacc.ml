(* $Id: pxp_yacc.ml,v 1.3 2003/06/20 21:00:33 gerd Exp $ -*- tuareg -*-
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Parsing
open Pxp_core_types
open Pxp_lexer_types
open Pxp_dtd
open Pxp_entity
open Pxp_entity_manager
open Pxp_document
open Pxp_aux
open Pxp_reader
open Netchannels

type config = Pxp_types.config =
    { warner : collect_warnings;
      swarner : symbolic_warnings option;
      enable_pinstr_nodes : bool;
      enable_super_root_node : bool;
      enable_comment_nodes : bool;
      drop_ignorable_whitespace : bool;
      encoding : rep_encoding;
      recognize_standalone_declaration : bool;
      store_element_positions : bool;
      idref_pass : bool;
      validate_by_dfa : bool;
      accept_only_deterministic_models : bool;
      disable_content_validation : bool;
      name_pool : Pxp_core_types.pool;
      enable_name_pool_for_element_types    : bool;
      enable_name_pool_for_attribute_names  : bool;
      enable_name_pool_for_attribute_values : bool;
      enable_name_pool_for_pinstr_targets   : bool;
      enable_namespace_processing : Pxp_dtd.namespace_manager option;
      enable_namespace_info : bool;
      escape_contents :
         (Pxp_lexer_types.token -> Pxp_entity_manager.entity_manager ->
                string) option;
      escape_attributes :
         (Pxp_lexer_types.token -> int -> Pxp_entity_manager.entity_manager ->
                string) option;
      debugging_mode : bool;
    }


let default_config = Pxp_types.default_config

let default_namespace_config = Pxp_types.default_namespace_config


type source = Pxp_types.source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (ext_id * Pxp_reader.resolver)
  | XExtID of (ext_id * string option * Pxp_reader.resolver)


let from_channel = Pxp_types.from_channel

let from_string = Pxp_types.from_string

let from_obj_channel = Pxp_types.from_obj_channel

let from_file = Pxp_types.from_file

exception ID_not_unique = Pxp_tree_parser.ID_not_unique

class type [ 'ext ] index = [ 'ext ] Pxp_tree_parser.index

class [ 'ext ] hash_index = [ 'ext ] Pxp_tree_parser.hash_index

let default_extension = Pxp_tree_parser.default_extension

let default_spec = Pxp_tree_parser.default_spec

let default_namespace_spec = Pxp_tree_parser.default_namespace_spec

let parse_document_entity = Pxp_tree_parser.parse_document_entity

let parse_wfdocument_entity = Pxp_tree_parser.parse_wfdocument_entity

let parse_content_entity = Pxp_tree_parser.parse_content_entity

let parse_wfcontent_entity = Pxp_tree_parser.parse_wfcontent_entity

let parse_dtd_entity = Pxp_dtd_parser.parse_dtd_entity

let extract_dtd_from_document_entity = 
  Pxp_dtd_parser.extract_dtd_from_document_entity


type event = Pxp_types.event =
  | E_start_doc of (string * bool * dtd)
  | E_end_doc
  | E_start_tag of (string * (string * string) list * Pxp_lexer_types.entity_id)
  | E_ns_start_tag of (string * string * (string * string * string) list *
		       Pxp_lexer_types.entity_id)
  | E_end_tag   of (string * Pxp_lexer_types.entity_id)
  | E_ns_end_tag of (string * string * Pxp_lexer_types.entity_id)
  | E_char_data of  string
  | E_pinstr of (string * string)
  | E_comment of string
  | E_position of (string * int * int)
  | E_error of exn
  | E_end_of_stream

let create_entity_manager = Pxp_ev_parser.create_entity_manager

type entry = Pxp_types.entry

let process_entity = Pxp_ev_parser.process_entity

let process_expr = Pxp_ev_parser.process_expr

let create_pull_parser = Pxp_ev_parser.create_pull_parser


(* ======================================================================
 * History:
 *
 * $Log: pxp_yacc.ml,v $
 * Revision 1.3  2003/06/20 21:00:33  gerd
 * 	Moved events to Pxp_types.
 * 	Implementation of namespaces in event-based parsers.
 *
 * Revision 1.2  2003/06/20 15:14:14  gerd
 * 	Introducing symbolic warnings, expressed as polymorphic
 * variants
 *
 * Revision 1.1  2003/06/15 18:18:34  gerd
 * 	Initial revision
 *
 * Revision 1.46  2003/06/15 12:23:22  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.45  2003/01/21 00:19:40  gerd
 * 	Support for the new resolvers.
 *
 * Revision 1.44  2002/10/22 14:23:12  gerd
 * 	Fix: Closing the topmost entities
 *
 * Revision 1.43  2002/08/31 23:28:01  gerd
 * 	Bugfix: It is checked whether all entities are closed when
 * the parser stops. If not, an error is generated.
 *
 * Revision 1.42  2002/08/17 23:51:56  gerd
 * 	Added pull parsing.
 *
 * Revision 1.41  2002/08/17 19:53:53  gerd
 * 	Changed type [entry] into a polymorphic variant. New variant
 * `Entry_expr. New: flags for [entry].
 * 	New: [process_expr].
 *
 * Revision 1.40  2002/08/05 22:34:29  gerd
 * 	escape_attributes: this config option has an additional
 * argument "position".
 *
 * Revision 1.39  2002/08/03 17:55:59  gerd
 * 	Support for event-based parsing of attribute values: New config
 * option escape_attributes.
 *
 * Revision 1.38  2002/07/14 23:46:29  gerd
 * 	Fix: Reverting to the old method, and creating a new
 * entity_manager to read the external DTD subset.
 *
 * Revision 1.37  2002/07/14 23:05:01  gerd
 * 	Event-based interface.
 *
 * Revision 1.36  2002/03/10 23:40:52  gerd
 * 	type source is now primarily defined in Pxp_dtd.
 *
 * Revision 1.35  2002/02/20 00:25:23  gerd
 * 	using Pxp_lexing instead of Lexing.
 *
 * Revision 1.34  2002/02/18 00:26:14  gerd
 * 	Small optimization in method save_data.
 *
 * Revision 1.33  2001/10/12 21:38:14  gerd
 * 	Changes for O'caml 3.03-alpha.
 *
 * Revision 1.32  2001/07/04 21:55:52  gerd
 * 	Bugfix: Early comments and processing instructions (i.e.
 * if they occur before the first element) are now handled correctly.
 *
 * Revision 1.31  2001/06/30 00:05:12  gerd
 * 	Fix: When checking the type of the root element, namespace
 * rewritings are taken into account.
 *
 * Revision 1.30  2001/06/29 14:44:35  gerd
 * 	Fixed: ~transform_dtd works now if enable_super_root
 *
 * Revision 1.29  2001/06/28 22:42:07  gerd
 * 	Fixed minor problems:
 * 	- Comments must be contained in one entity
 * 	- Pxp_document.document is now initialized with encoding.
 *           the DTD encoding may be initialized too late.
 *
 * Revision 1.28  2001/06/09 22:32:24  gerd
 * 	Fixed the way set_namespace_info is called.
 *
 * Revision 1.27  2001/06/08 01:15:47  gerd
 * 	Moved namespace_manager from Pxp_document to Pxp_dtd. This
 * makes it possible that the DTD can recognize the processing instructions
 * <?pxp:dtd namespace prefix="..." uri="..."?>, and add the namespace
 * declaration to the manager.
 *
 * Revision 1.26  2001/06/07 22:55:14  gerd
 * 	Uses methods classify_data_node, append_node, validate_contents
 * now provided by nodes.
 *
 * Revision 1.25  2001/05/17 22:39:10  gerd
 * 	Fix: default_spec
 *
 * Revision 1.24  2001/05/17 21:39:31  gerd
 * 	Initial implementation of namespace parsing.
 *
 * Revision 1.23  2001/04/27 00:00:14  gerd
 * 	Added a comment what to do to implement namespaces. See
 * the rule start_tag.
 *
 * Revision 1.22  2001/04/24 21:07:13  gerd
 * 	New option ~alt in from_channel and from_file.
 *
 * Revision 1.21  2001/04/22 15:15:40  gerd
 * 	Improved error messages.
 *
 * Revision 1.20  2001/04/22 14:17:35  gerd
 * 	from_channel uses now standard features of Pxp_reader, and
 * is no longer a hack.
 *
 * Revision 1.19  2001/04/03 20:22:44  gerd
 * 	New resolvers for catalogs of PUBLIC and SYSTEM IDs.
 * 	Improved "combine": PUBLIC and SYSTEM IDs are handled
 * separately.
 * 	Rewritten from_file: Is now a simple application of the
 * Pxp_reader classes and functions. (The same has still to be done
 * for from_channel!)
 *
 * Revision 1.18  2000/10/01 19:49:04  gerd
 * 	Many small optimizations, espcially in attribute parsing.
 * 	New type array_stack.
 *
 * Revision 1.17  2000/09/21 21:30:46  gerd
 * 	New option: disable_content_validation
 *
 * Revision 1.16  2000/09/16 22:48:23  gerd
 * 	Failure "Invalid UTF-8 stream" which may raised by wlex-
 * generated code is converted to Malformed_code.
 * 	Instead of dtd#allow_arbitrary the corresponding
 * processing instruction is added to the DTD. Advantage: When
 * marshalled, this property will not be lost.
 *
 * Revision 1.15  2000/09/09 16:41:03  gerd
 * 	Effort to reduce the amount of allocated memory: The number of
 * instance variables in document nodes has been miminized; the class
 * default_ext no longer stores anything; string pools have been implemented.
 *
 * Revision 1.14  2000/08/26 23:23:14  gerd
 * 	Bug: from_file must not interpret the file name as URL path.
 * 	Bug: When PI and comment nodes are generated, the collected data
 * material must be saved first.
 *
 * Revision 1.13  2000/08/19 21:30:03  gerd
 * 	Improved the error messages of the parser
 *
 * Revision 1.12  2000/08/18 20:16:25  gerd
 * 	Implemented that Super root nodes, pinstr nodes and comment
 * nodes are included into the document tree.
 *
 * Revision 1.11  2000/08/14 22:24:55  gerd
 * 	Moved the module Pxp_encoding to the netstring package under
 * the new name Netconversion.
 *
 * Revision 1.10  2000/07/23 02:16:33  gerd
 * 	Support for DFAs.
 *
 * Revision 1.9  2000/07/14 13:57:29  gerd
 * 	Added the id_index feature.
 *
 * Revision 1.8  2000/07/09 17:52:45  gerd
 * 	New implementation for current_data.
 * 	The position of elements is stored on demand.
 *
 * Revision 1.7  2000/07/09 01:00:35  gerd
 * 	Improvement: It is now guaranteed that only one data node
 * is added for consecutive character material.
 *
 * Revision 1.6  2000/07/08 16:27:29  gerd
 * 	Cleaned up the functions calling the parser.
 * 	New parser argument: transform_dtd.
 * 	Implementations for 'extract_dtd_from_document_entity' and
 * 'parse_wfcontent_entity'.
 *
 * Revision 1.5  2000/07/06 23:05:18  gerd
 * 	Initializations of resolvers were missing.
 *
 * Revision 1.4  2000/07/06 22:11:01  gerd
 * 	Fix: The creation of the non-virtual root element is protected
 * in the same way as the virtual root element.
 *
 * Revision 1.3  2000/07/04 22:15:18  gerd
 * 	Change: Using the new resolver capabilities.
 * 	Still incomplete: the new extraction and parsing functions.
 *
 * Revision 1.2  2000/06/14 22:19:06  gerd
 * 	Added checks such that it is impossible to mix encodings.
 *
 * Revision 1.1  2000/05/29 23:48:38  gerd
 * 	Changed module names:
 * 		Markup_aux          into Pxp_aux
 * 		Markup_codewriter   into Pxp_codewriter
 * 		Markup_document     into Pxp_document
 * 		Markup_dtd          into Pxp_dtd
 * 		Markup_entity       into Pxp_entity
 * 		Markup_lexer_types  into Pxp_lexer_types
 * 		Markup_reader       into Pxp_reader
 * 		Markup_types        into Pxp_types
 * 		Markup_yacc         into Pxp_yacc
 * See directory "compatibility" for (almost) compatible wrappers emulating
 * Markup_document, Markup_dtd, Markup_reader, Markup_types, and Markup_yacc.
 *
 * ======================================================================
 * Old logs from markup_yacc.m2y:
 *
 * Revision 1.9  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.8  2000/05/27 19:26:19  gerd
 * 	Change: The XML declaration is interpreted right after
 * it has been parsed (no longer after the document): new function
 * check_and_parse_xmldecl.
 * 	When elements, attributes, and entities are declared
 * it is stored whether the declaration happens in an external
 * entity (for the standalone check).
 * 	The option recognize_standalone_declaration is interpreted.
 *
 * Revision 1.7  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.6  2000/05/14 21:51:24  gerd
 * 	Change: Whitespace is handled by the grammar, and no longer
 * by the entity.
 *
 * Revision 1.5  2000/05/14 17:50:54  gerd
 * 	Updates because of changes in the token type.
 *
 * Revision 1.4  2000/05/11 22:09:17  gerd
 * 	Fixed the remaining problems with conditional sections.
 * This seems to be also a weakness of the XML spec!
 *
 * Revision 1.3  2000/05/09 00:02:44  gerd
 * 	Conditional sections are now recognized by the parser.
 * There seem some open questions; see the TODO comments!
 *
 * Revision 1.2  2000/05/08 22:01:44  gerd
 * 	Introduced entity managers (see markup_entity.ml).
 * 	The XML declaration is now recognized by the parser. If such
 * a declaration is found, the method process_xmldecl of the currently
 * active entity is called. If the first token is not an XML declaration,
 * the method process_missing_xmldecl is called instead.
 * 	Some minor changes.
 *
 * Revision 1.1  2000/05/06 23:21:49  gerd
 * 	Initial revision.
 *
 *
 * ======================================================================
 *
 * COPIED FROM REVISION 1.19 OF markup_yacc.mly
 *
 * Revision 1.19  2000/05/01 15:20:08  gerd
 * 	"End tag matches start tag" is checked before "End tag in the
 * same entity as start tag".
 *
 * Revision 1.18  2000/04/30 18:23:08  gerd
 * 	Bigger change: Introduced the concept of virtual roots. First,
 * this reduces the number of checks. Second, it makes it possible to
 * return the virtual root to the caller instead of the real root (new
 * config options 'virtual_root' and 'processing_instructions_inline').
 * 	Minor changes because of better CR/CRLF handling.
 *
 * Revision 1.17  2000/03/13 23:47:46  gerd
 * 	Updated because of interface changes. (See markup_yacc_shadow.mli
 * rev. 1.8)
 *
 * Revision 1.16  2000/01/20 20:54:43  gerd
 * 	New config.errors_with_line_numbers.
 *
 * Revision 1.15  1999/12/17 22:27:58  gerd
 * 	Bugfix: The value of 'p_internal_subset' (an instance
 * variable of the parser object) is to true when the internal subset
 * begins, and is set to false when this subset ends. The error was
 * that references to external entities within this subset did not
 * set 'p_internal_subset' to false; this is now corrected by introducing
 * the 'p_internal_subset_stack'.
 * 	This is a typical example of how the code gets more and
 * more complicated and that it is very difficult to really understand
 * what is going on.
 *
 * Revision 1.14  1999/11/09 22:23:37  gerd
 * 	Removed the invocation of "init_dtd" of the root document.
 * This method is no longer available. The DTD is also passed to the
 * document object by the root element, so nothing essential changes.
 *
 * Revision 1.13  1999/10/25 23:37:09  gerd
 * 	Bugfix: The warning "More than one ATTLIST declaration for element
 * type ..." is only generated if an ATTLIST is found while there are already
 * attributes for the element.
 *
 * Revision 1.12  1999/09/01 23:08:38  gerd
 * 	New frontend function: parse_wf_document. This simply uses
 * a DTD that allows anything, and by the new parameter "extend_dtd" it is
 * avoided that element, attlist, and notation declarations are added to this
 * DTD. The idea is that this function simulates a well-formedness parser.
 * 	Tag_beg, Tag_end carry the entity_id. The "elstack" stores the
 * entity_id of the stacked tag. This was necessary because otherwise there
 * are some examples to produces incorrectly nested elements.
 * 	p_internal_subset is a variable that stores whether the internal
 * subset is being parsed. This is important beacause entity declarations in
 * internal subsets are not allowed to contain parameter references.
 * 	It is checked if the "elstack" is empty after all has been parsed.
 * 	Processing instructions outside DTDs and outside elements are now
 * added to the document.
 * 	The rules of mixed and regexp style content models have been
 * separated. The code is now much simpler.
 * 	Entity references outside elements are detected and rejected.
 *
 * Revision 1.11  1999/09/01 16:26:08  gerd
 * 	Improved the quality of error messages.
 *
 * Revision 1.10  1999/08/31 19:13:31  gerd
 * 	Added checks on proper PE nesting. The idea is that tokens such
 * as Decl_element and Decl_rangle carry an entity ID with them. This ID
 * is simply an object of type < >, i.e. you can only test on identity.
 * The lexer always produces tokens with a dummy ID because it does not
 * know which entity is the current one. The entity layer replaces the dummy
 * ID with the actual ID. The parser checks that the IDs of pairs such as
 * Decl_element and Decl_rangle are the same; otherwise a Validation_error
 * is produced.
 *
 * Revision 1.9  1999/08/15 20:42:01  gerd
 * 	Corrected a misleading message.
 *
 * Revision 1.8  1999/08/15 20:37:34  gerd
 * 	Improved error messages.
 * 	Bugfix: While parsing document entities, the subclass document_entity is
 * now used instead of external_entity. The rules in document entities are a bit
 * stronger.
 *
 * Revision 1.7  1999/08/15 14:03:59  gerd
 * 	Empty documents are not allowed.
 * 	"CDATA section not allowed here" is a WF_error, not a Validation_
 * error.
 *
 * Revision 1.6  1999/08/15 02:24:19  gerd
 * 	Removed some grammar rules that were used for testing.
 * 	Documents without DTD can now have arbitrary elements (formerly
 * they were not allowed to have any element).
 *
 * Revision 1.5  1999/08/14 22:57:20  gerd
 * 	It is allowed that external entities are empty because the
 * empty string is well-parsed for both declarations and contents. Empty
 * entities can be referenced anywhere because the references are replaced
 * by nothing. Because of this, the Begin_entity...End_entity brace is only
 * inserted if the entity is non-empty. (Otherwise references to empty
 * entities would not be allowed anywhere.)
 * 	As a consequence, the grammar has been changed such that a
 * single Eof is equivalent to Begin_entity,End_entity without content.
 *
 * Revision 1.4  1999/08/14 22:20:01  gerd
 *         The "config" slot has now a component "warner" which is
 * an object with a "warn" method. This is used to warn about characters
 * that cannot be represented in the Latin 1 alphabet.
 *         Furthermore, there is a new component "debugging_mode".
 *         Some Parse_error exceptions have been changed into Validation_error.
 *         The interfaces of functions/classes imported from other modules
 * have changed; the invocations have been adapted.
 *         Contents may contain CDATA sections that have been forgotten.
 *
 * Revision 1.3  1999/08/11 15:00:41  gerd
 * 	The Begin_entity ... End_entity brace is also possible in
 * 'contents'.
 * 	The configuration passed to the parsing object contains always
 * the resolver that is actually used.
 *
 * Revision 1.2  1999/08/10 21:35:12  gerd
 * 	The XML/encoding declaration at the beginning of entities is
 * evaluated. In particular, entities have now a method "xml_declaration"
 * which returns the name/value pairs of such a declaration. The "encoding"
 * setting is interpreted by the entity itself; "version", and "standalone"
 * are interpreted by Markup_yacc.parse_document_entity. Other settings
 * are ignored (this does not conform to the standard; the standard prescribes
 * that "version" MUST be given in the declaration of document; "standalone"
 * and "encoding" CAN be declared; no other settings are allowed).
 * 	TODO: The user should be warned if the standard is not exactly
 * fulfilled. -- The "standalone" property is not checked yet.
 *
 * Revision 1.1  1999/08/10 00:35:52  gerd
 * 	Initial revision.
 *
 *
 *)
