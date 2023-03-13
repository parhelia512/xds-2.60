(** Copyright (c) 1991,2023 Excelsior Ltd, Novosibirsk, Russia. All Rights Reserved. *)
(** XDS LLVM back-end: target platform tune-ups                                      *)
MODULE llTune; (* Hady 13-Mar-2023. *)

VAR
  BITS32: BOOLEAN;
  
CONST
  BITS_PER_LOC *= 8;
  jmp_buf_size  = 180; (* !!!! *)
  empty_rec_size *= 4;
  
VAR WORD_SIZE-: LONGINT;
  
VAR
  default_alignment-: SHORTINT;
  
VAR
  BITSET_LEN -: LONGINT;  
  
VAR
  char_sz    -: LONGINT;
  bitset_sz  -: LONGINT; (** size of packed set *)
  addr_sz    -: LONGINT;   
  proc_sz    -: LONGINT;  
  real_sz    -: LONGINT;
  longreal_sz-: LONGINT;
  ld_real_sz -: LONGINT; (** longlongreal *)
  process_size -: LONGINT;

PROCEDURE setAlignment;
BEGIN
  default_alignment:=4;
END setAlignment;

PROCEDURE init32;
BEGIN
  setAlignment;
  WORD_SIZE := 4;
  BITSET_LEN:= WORD_SIZE*BITS_PER_LOC;
  
  char_sz := 1;
  bitset_sz := WORD_SIZE;
  addr_sz   := WORD_SIZE;
  proc_sz   := WORD_SIZE;
  real_sz   := 4;
  longreal_sz := 8;
  ld_real_sz  := 10; (* for x86 platform shall work; for nother platforms invesigation needed *);
  process_size := 12+jmp_buf_size;
    
END init32;

PROCEDURE init64;
BEGIN
  ASSERT(FALSE);
END init64;

BEGIN
  BITS32:=TRUE;
  IF BITS32 THEN init32 ELSE init64 END;
END llTune.