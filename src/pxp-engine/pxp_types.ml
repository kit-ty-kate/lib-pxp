(* $Id: pxp_types.ml,v 1.16 2003/06/15 12:23:22 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

include Pxp_core_types




(* ======================================================================
 * History:
 *
 * $Log: pxp_types.ml,v $
 * Revision 1.16  2003/06/15 12:23:22  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.15  2003/01/21 00:18:09  gerd
 * 	New type resolver_id. It is related to ext_id but contains
 * more information.
 *
 * Revision 1.14  2002/08/28 23:54:34  gerd
 * 	Support for new lexer definition style.
 *
 * Revision 1.13  2001/06/27 23:33:53  gerd
 * 	Type output_stream is now a polymorphic variant
 *
 * Revision 1.12  2001/06/07 22:49:51  gerd
 * 	New namespace exceptions.
 *
 * Revision 1.11  2001/04/26 23:57:04  gerd
 * 	New exception Method_not_applicable. It is raised if there are
 * classes A and B both conforming to class type C, but A does not implement
 * a method required by the class type. In this case, invoking the method
 * in A raises Method_not_applicable.
 * 	This feature is mainly used in Pxp_document.
 *
 * Revision 1.10  2001/04/22 14:14:41  gerd
 * 	Updated to support private IDs.
 *
 * Revision 1.9  2000/09/17 00:12:19  gerd
 * 	Bugfix in the pool implementation.
 *
 * Revision 1.8  2000/09/09 16:38:47  gerd
 * 	New type 'pool'.
 *
 * Revision 1.7  2000/08/14 22:24:55  gerd
 * 	Moved the module Pxp_encoding to the netstring package under
 * the new name Netconversion.
 *
 * Revision 1.6  2000/07/27 00:41:15  gerd
 * 	new 8 bit codes
 *
 * Revision 1.5  2000/07/16 18:31:09  gerd
 * 	The exception Illegal_character has been dropped.
 *
 * Revision 1.4  2000/07/14 21:25:27  gerd
 * 	Simplified the type 'collect_warnings'.
 *
 * Revision 1.3  2000/07/08 16:23:50  gerd
 * 	Added the exception 'Error'.
 *
 * Revision 1.2  2000/07/04 22:14:05  gerd
 * 	Implemented the changes of rev. 1.2 of pxp_types.mli.
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
 * Old logs from markup_types.ml:
 *
 * Revision 1.7  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.6  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.5  2000/05/01 20:43:19  gerd
 * 	New type output_stream; new function 'write'.
 *
 * Revision 1.4  1999/09/01 16:25:35  gerd
 * 	Dropped Illegal_token and Content_not_allowed_here. WF_error can
 * be used instead.
 *
 * Revision 1.3  1999/08/15 02:22:33  gerd
 * 	Added exception Undeclared.
 *
 * Revision 1.2  1999/08/14 22:14:58  gerd
 * 	New class "collect_warnings".
 *
 * Revision 1.1  1999/08/10 00:35:52  gerd
 * 	Initial revision.
 *
 *
 *)
