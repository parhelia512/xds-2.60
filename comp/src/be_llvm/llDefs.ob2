(** Copyright (c) 1991,2023 Excelsior Ltd, Novosibirsk, Russia. All Rights Reserved. *)
(** XDS LLVM back-end: Data structures definitions                                   *)
(** This module is sufficiently based on x86 back-end (!!!) *)
MODULE llDefs;  (* Hady 02-Jan-2023. *)

(** 
  The code of each procedure is transformed to LLVM assembler level representation
  which is a graph of basic blocks. 
  Each basic block is a linear sequence of instructions that is terminated by a jump
  to the the next basic block in order of execution.
  Basic blocks are kept in the Nodes table and the Node ID is an index in this table.
  Instructions of the basic block are organized as a two-tier list.
*)

IMPORT pcK;
IMPORT DStrings;

TYPE String = DStrings.String;
  
CONST  (** LLVM instructions https://llvm.org/docs/LangRef.html#instruction-reference *)
  iUndef  *=  0; (* seems may be needed for code safety *)
(** Terminator instructions *)  
  iRet    *=  1; (** ret <type> <value>  | ret void  *)
  iBr     *=  2; (** br i1 <cond>, label <iftrue>, label <iffalse>  |  br label <dest> *)
  iSwitch *=  3; (** switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ] *)
  iIndirectbr *= 4;  (** indirectbr ptr <address>, [ label <dest1>, label <dest2>, ... ] *)
  iInvoke *=  5; (** invoke [cconv] [ret attrs] [addrspace(<num>)] <ty>|<fnty> <fnptrval>(<function args>) [fn attrs]
                     [operand bundles] to label <normal label> unwind label <exception label> *)
  iCallbr *=  6; (** call function which handles exceptions ?
                     callbr [cconv] [ret attrs] [addrspace(<num>)] <ty>|<fnty> <fnptrval>(<function args>) [fn attrs]
                     [operand bundles] to label <fallthrough label> [indirect labels] *)
  iResume *=  7; (** jump to the next exception landing pad; "resume <type> <value>" *)
  iCatchswitch*=  8;  (** exception handling
                          <resultval> = catchswitch within <parent> [ label <handler1>, label <handler2>, ... ] unwind to caller
                          <resultval> = catchswitch within <parent> [ label <handler1>, label <handler2>, ... ] unwind label <default> *)
  iCatchret   *=  9;  (** exception handling;  catchret from <token> to label <normal> *)
  iCleanupret *= 10;  (** exception handling;  cleanupret from <value> unwind label <continue>
                                               cleanupret from <value> unwind to caller *)
  iUnreachable*= 11;  (** "unreachable" *)
(** Unary Operations *)
  iFneg   *= 12; (** <result> = fneg [fast-math flags]* <ty> <op1>   ; yields ty:result *)
(** Binary Operations *)
  iAdd    *= 13; (** <result> = add  [nuw | nsw| nuw nsw] <ty> <op1>, <op2>  ; yields ty:result *)
  iFadd   *= 14; (** <result> = fadd [fast-math flags]* <ty> <op1>, <op2>   ; yields ty:result *)
  iSub    *= 15; (** <result> = sub  [nuw | nsw| nuw nsw] <ty> <op1>, <op2>  ; yields ty:result *)
  iFsub   *= 16; (** <result> = fsub [fast-math flags]* <ty> <op1>, <op2>   ; yields ty:result *)
  iMul    *= 17; (** <result> = mul  [nuw | nsw| nuw nsw] <ty> <op1>, <op2>  ; yields ty:result *)
  iFmul   *= 18; (** <result> = fmul [fast-math flags]* <ty> <op1>, <op2>   ; yields ty:result *)
  iUdiv   *= 19; (** <result> = udiv [exact] <ty> <op1>, <op2>   ; yields ty:result -- unsigned *)
  iSdiv   *= 20; (** <result> = sdiv [exact] <ty> <op1>, <op2>   ; yields ty:result -- signed *)
  iFdiv   *= 21; (** <result> = fdiv [fast-math flags]* <ty> <op1>, <op2>   ; yields ty:result *) 
  iUrem   *= 22; (** <result> = urem <ty> <op1>, <op2>   ; yields ty:result -- unsigned *)
  iSrem   *= 23; (** <result> = srem <ty> <op1>, <op2>   ; yields ty:result -- signed *)
  iFrem   *= 24; (** <result> = frem [fast-math flags]* <ty> <op1>, <op2>   ; yields ty:result -- !!! not needed? *)
(** Bitwise Binary Operations *)
  iShl    *= 25; (** <result> = shl  [nuw] [nsw] <ty> <op1>, <op2>   ; yields ty:result arythmetics shift left *)
  iLshr   *= 26; (** <result> = lshr [nuw] [nsw] <ty> <op1>, <op2>   ; yields ty:result logical shift right *)
  iAshr   *= 27; (** <result> = ashr [nuw] [nsw] <ty> <op1>, <op2>   ; yields ty:result arythmetics shift right *)
  iAnd    *= 28; (** <result> = and  <ty> <op1>, <op2>   ; yields ty:result *)
  iOr     *= 29; (** <result> = or   <ty> <op1>, <op2>   ; yields ty:result *)
  iXor    *= 30; (** <result> = xor  <ty> <op1>, <op2>   ; yields ty:result *)
(** Vector Operations. Currently not needed, but may be needed in future, so they are here *)
  iExtractelement *= 31; (** <result> = extractelement < [vscale x] n x <ty>> <val>, <ty2> <idx>  ; yields <ty> *)
  iInsertelement  *= 32; (** <result> = insertelement < [vscale x] n x <ty>> <val>, <ty> <elt>, <ty2> <idx> ; yields < [vscale x] n x <ty>> *) 
  iShufflevector  *= 33; (** <result> = shufflevector <n x <ty>> <v1>, <n x <ty>> <v2>, <m x i32> <mask>    ; yields <m x <ty>> *)
(** Aggregate Operations. Not sure if they are needed currently either as Vector Operations *)
  iExtractvalue   *= 34; (** <result> = extractvalue <aggregate type> <val>, <idx>{, <idx>}* *)
  iInsertvalue    *= 35; (** <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>{, <idx>}*    ; yields <aggregate type> *)
(** Memory Access and Addressing Operations *)
  iAlloca *= 36; (** <result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] 
                                       [, addrspace(<num>)]     ; yields type addrspace(num)*:result *)
  iLoad   *= 37; (** <result> = load [volatile] <ty>, ptr <pointer>[, align <alignment>] -- more info https://llvm.org/docs/LangRef.html#id1876 *)                                       
  iStore  *= 38; (** store [volatile] <ty> <value>, ptr <pointer>[, align <alignment>]   -- more info https://llvm.org/docs/LangRef.html#id1877 *)
  iFence  *= 39; (** fence [syncscope("<target-scope>")] <ordering>  ; yields void  -- more info https://llvm.org/docs/LangRef.html#id1878 *)
  iCmpxchg        *= 40; (** Compare and xchange. Details at: https://llvm.org/docs/LangRef.html#id1879 *)
  iGetelemptr     *= 41; (** <result> = getelementptr [inbounds] <ty>, ptr <ptrval>{, [inrange] <ty> <idx>}*  More at: https://llvm.org/docs/LangRef.html#id1881 *)
(** Conversion operations *)
  iTrunc  *= 42; (** <result> = trunc <ty> <value> to <ty2>  *)
  iZext   *= 43; (** <result> = zext <ty> <value> to <ty2> -- zero extend *)
  iSext   *= 44; (** <result> = sext <ty> <value> to <ty2> -- sign extend *)
  iFptrunc*= 45; (** <result> = fptrunc <ty> <value> to <ty2> -- floating point trunc *)
  iFpext  *= 46; (** <result> = fpext <ty> <value> to <ty2> *)
  iFptoui *= 47; (** <result> = fptoui <ty> <value> to <ty2> -- float to unsigned *)
  iFptosi *= 48; (** <result> = fptosi <ty> <value> to <ty2> -- float to signed *)
  iUitofp *= 49; (** <result> = uitofp <ty> <value> to <ty2> -- unsigned to float *)
  iSitofp *= 50; (** <result> = sitofp <ty> <value> to <ty2> -- signed to float *)
  iPtrtoint       *= 51; (** <result> = ptrtoint <ty> <value> to <ty2> *)
  iInttoptr       *= 52; (** <result> = inttoptr <ty> <value> to <ty2>  -- More at: https://llvm.org/docs/LangRef.html#id1893 *)
  iBitcast        *= 53; (** <result> = bitcast <ty> <value> to <ty2> *) 
  iAddrspacecast  *= 54; (** <result> = addrspacecast <pty> <ptrval> to <pty2> *)
(** Other Operations *)
  iIcmp   *= 55; (** <result> = icmp < eq|ne|ugt|uge|ult|ule|sgt|sge|slt|sle > <ty> <op1>, <op2>  *)
  iFcmp   *= 56; (** <result> = fcmp [fast-math flags]* <cond> <ty> <op1>, <op2> -- More at: https://llvm.org/docs/LangRef.html#id1898 *)
  iPhi    *= 57; (** <result> = phi [fast-math-flags] <ty> [ <val0>, <label0>], ... *)
  iSelect *= 58; (** <result> = select [fast-math flags] selty <cond>, <ty> <val1>, <ty> <val2> select one of two args with no IR branch *)
  iFreeze *= 59; (** <result> = freeze ty <val>  -- stops "undef" or "poison" propogation *)
  iCall   *= 60; (** <result> = [tail | musttail | notail ] call [fast-math flags] [cconv] [ret attrs] [addrspace(<num>)]
                     <ty>|<fnty> <fnptrval>(<function args>) [fn attrs] [ operand bundles ]
                     More at: https://llvm.org/docs/LangRef.html#id1902 *)
  iVa_arg *= 61; (** <resultval> = va_arg <va_list*> <arglist>, <argty> *)
  iLandingpad     *= 62; (** See at https://llvm.org/docs/LangRef.html#id1904 *)
  iCatchpad       *= 63; (** See at https://llvm.org/docs/LangRef.html#id1905 *)
  iCleanuppad     *= 64; (** See at https://llvm.org/docs/LangRef.html#id1906 *)                   
    
TYPE
  Instruction* = POINTER TO InstrDesc;
  
TYPE TypeTag *= (   (* !!! not sure if it is needed *)
       ttUndef,
       ttVoid,
       ttInt,
       ttPtr
  );
  
TYPE InstrOption *= (
    optNoSignedOverflow,    (** for int arithmetic's *)
    optNoUnsignedOverflow,  (** for int arithmetic's *)
    optVoidFunc             (** for function calls to simplify result type selection *)
  );
  
TYPE InstrOptions *= PACKEDSET OF InstrOption;
  
TYPE
  Arg *= RECORD (** Type to represent LLVM instruction result and arguments *)
    name*: String;   --* name of type, label or variable/value
    type*: String;   --* type name to prefix variable
    align*: LONGINT; --* alignment
  END;
    
TYPE 
  InstrDesc* = RECORD         (** type to store info for LLVM instuctions  *)
    next*, prev*: Instruction;  (** Instructions are linked in two-tier list *)
    op*  : LONGINT;             (** see instructions constants above *)
    opts*: InstrOptions;
    node*: LONGINT;
    pos* : pcK.TPOS;
    res* : Arg;
    args*: POINTER TO ARRAY OF Arg;
  END; 

TYPE 
  NodeDesc* = RECORD
    first*, last*: Instr; (** basic block of this Node *)
    label*: String;
  END;
  NodeTable* = POINTER TO ARRAY OF NodesDesc;
  
CONST
  UndefNode = MAX(LONGINT); 
  
VAR
  Nodes* : NodeTable;  (** Nodes of the current procedure. Node ID == index in this table *)
  NNodes*: LONGINT;    (** current number of active nodes *)
 
(* Basic block two-tier list manipulations. Made as methods for convenient typing *)  
 
PROCEDURE (q: Instruction) PutFirst* (n: Node);
(** add this instruction to the beginning of basic block of n *)
BEGIN
  q.next := Nodes[n].first;
  q.prev := NIL;
  q.node := n;
  Nodes[n].first := q;
  IF q.next#NIL THEN q.next.prev := q;
  ELSE Nodes[n].last := q;
  END;
END PutFirst;

PROCEDURE (q: Instruction) PutLast* (n: Node);
(** add this instruction to the end of basic block of n *)
BEGIN
  q.prev := Nodes[n].last;
  q.next := NIL;
  q.node := n;
  Nodes[n].last := q;
  IF q.prev#NIL THEN q.prev.next:=q;
  ELSE Nodes[n].first:=q;
  END;
END PutLast;
 
PROCEDURE (q: Instruction) InsertBefore* (p: Instruction);
(** insert this instruction before p *)
BEGIN
  IF p.prev = NIL THEN Nodes[p.node].first:=q;
  ELSE p.prev.next:=q;
  END;
  q.next := p;
  q.prev := p.prev;
  p.prev := q;
  q.node := p.node;
END InsertBefore;

PROCEDURE (q: Instruction) InsertAfter* (p : Instruction);
(** insert this instruction after p *)
BEGIN
  IF p.next = NIL THEN Nodes[p.node].last := q;
  ELSE p.next.prev := q;
  END;
  q.prev := p;
  q.next := p^.Next;
  p.next := q;
  q.node := p.node;
END InsertAfter;

PROCEDURE  (p: Instruction) Remove*;
(** remove p from its basic block *)
BEGIN
  IF p.next = NIL THEN
    Nodes[p.node].last := p.prev;
  ELSE
    p.next.prev := p.prev;
  END;
  IF p.prev = NIL THEN
    Nodes[p.node].first := p.next;
  ELSE
    p.prev.next := p.next;
  END;
  p.node := UndefNode;
END Remove;

PROCEDURE (VAR n: Node) init;
(* not exported because it is not supposed to create nodes outside the NodesTable *)
BEGIN
  n.first:=NIL; n.last:=NIL;
END init;

PROCEDURE NewNode*(): LONGINT;
  VAR new: NodeTable;
      i,n: LONGINT;
BEGIN
  IF NNodes=LEN(Nodes^) THEN
    NEW(new, LEN(Nodes^)*2);
    FOR i:=0 TO LEN(Nodes^)-1 DO new[i]:=Nodes[i] END;
  END;
  n:=NNodes; INC(NNodes);
  Nodes[n].init;
  RETURN n;
END NewNode;

PROCEDURE NewGraph*;
(** reset state of this module to start new procedure transformation *)
BEGIN
  NEW(Nodes,10);
  NNodes:=0;
END NewGraph;

BEGIN
  Nodes:=NIL;
END llDefs.