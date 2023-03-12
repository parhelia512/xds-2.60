(** Copyright (c) 1991,2023 Excelsior Ltd, Novosibirsk, Russia. All Rights Reserved. *)
(** XDS LLVM back-end: Main module of the back-end                                   *)
(** This module is sufficiently based on x86 back-end, esp. opCode.ob2 (!!!) *)
MODULE llCode; (* Hady 01-Mar-2023. *)

IMPORT
  pc  := pcK,
  llt := llTypes;

(* ----------------- Backend interface  ------------------- *)

TYPE
  CODE = POINTER TO RECORD (pc.code_rec) END;

VAR
  code : CODE; (* back-end *)
  
PROCEDURE ( c: CODE ) ini*;
(*
*)
BEGIN ASSERT( FALSE );
END ini;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) exi*;
(*
*)
BEGIN
END exi;

-----------------------------------------------------------------------------

PROCEDURE (c: CODE ) set_min_value*( t: STRUCT; VAR v: VALUE );
BEGIN
END set_min_value;

-----------------------------------------------------------------------------

PROCEDURE (c: CODE ) set_max_value*( t: STRUCT; VAR v: VALUE );
BEGIN
END set_max_value;

-----------------------------------------------------------------------------


PROCEDURE ( c: CODE ) get_size* (kind: SUB_MODE;
                                 type: STRUCT
                                ): LONGINT;
(**
    Returns size of type.
    kind - one of: su_bits, su_bytes, su_size;
    Returns -1 if size is not defined in compile time.
  *)
  VAR sz: LONGINT;
BEGIN
  sz := llt.type_size(t);
  CASE op OF
    |pc.su_bits : IF sz > 0 THEN sz := sz * 8 END;
    |pc.su_bytes:
    |pc.su_size :
  END;
  RETURN sz
   
END get_size;

-----------------------------------------------------------------------------

PROCEDURE (c: CODE) get_offs* ( op: SUB_MODE
                              ;  o: OBJECT
                              )   :           LONGINT;
(*
   Returns offset of object.
   kind - one of: su_bits_offs, su_byte_offs, su_word_offs;
   Returns -1 if offs is not defined in compile time.
*)
BEGIN
  ASSERT( FALSE );
END get_offs;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) get_align* ( type: STRUCT ): SHORTINT;
(*
   Returns actual alignment of a type.
*)
BEGIN
  RETURN llt.type_align(t)
END get_align;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) allocate* (       cu: Mno
                                ;     main: BOOLEAN
                                ; src_time: xfs.Time );
(**
    Allocation of public objects of mods[cu] module;
    is called from front-end before storing symbol file.
*)
BEGIN
END allocate;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) out_object* ( file: xfs.SymFile
                                  ;    o: OBJECT );
(**
    Write platform dependent object attributes to symbol file.
*)
BEGIN ASSERT( FALSE );
END out_object;


PROCEDURE ( c: CODE ) clear_object* (o :OBJECT );
(**
    Clear module-specific platform-dependent object attributes
*)
BEGIN ASSERT( FALSE );
END clear_object;


-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) inp_object* ( file: xfs.SymFile
                                  ;    o: OBJECT
                                  ;   id: LONGINT );
(**
    Reads platform dependent object attributes from symbol file.
*)
BEGIN ASSERT( FALSE );
END inp_object;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) skip_object* ( file: xfs.SymFile
                                   ;   id: LONGINT );
(**
    Skips platform dependent object attributes in symbol file.
*)
BEGIN ASSERT( FALSE );
END skip_object;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) inp_struct* ( file: xfs.SymFile
                                  ;    s: STRUCT
                                  ;   id: LONGINT );
(**
    Reads platform dependent structure attributes from symbol file.
*)
BEGIN ASSERT( FALSE );
END inp_struct;

-----------------------------------------------------------------------------

PROCEDURE ( c: CODE ) skip_struct* ( file: xfs.SymFile
                                   ;   id: LONGINT );
(**
    Skips platform dependent structure attributes in symbol file.
*)
BEGIN ASSERT( FALSE );
END skip_struct;

-----------------------------------------------------------------------------

BEGIN
END llCode.