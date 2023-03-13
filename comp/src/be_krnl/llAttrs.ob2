(** Copyright (c) 1991,2023 Excelsior Ltd, Novosibirsk, Russia. All Rights Reserved. *)
(** XDS LLVM back-end: Manipulations with back-end extensions to the AST             *)
(** This module is sufficiently based on opAttrs.ob2 from  x86 back-end *)
MODULE llAttrs; (* Hady 06-Mar-2023. *)

IMPORT
  pc := pcK;
  
CONST
  otag_declared*   = pc.otag_aux1; (* declared, may be not defined        *)
  otag_declaring*  = pc.otag_aux2; (* declaration in progress             *)
  otag_defining*   = pc.otag_aux3; (* definition in progress              *)
  otag_undef*      = pc.otag_aux4; (* declared but not defined            *)

  (* значения tmark используются только во время одной трансляции *)
  (* и в sym-файлы не записываются                                *)
  tmark_processed *= pc.tmark_aux10; (* тип обработан   *)
  

TYPE
  Attr *= POINTER TO AttrRec; (** basic extension for all my attrs *)
  AttrRec *= RECORD(pc.bext_rec)
    next  : Attr;
    kind *: SHORTINT; (** See "a_*" constants below *)
  END;
  
PROCEDURE attr*(bex: pc.BEXT; kind: SHORTINT): Attr;
  VAR att: Attr;
BEGIN
  IF bex = NIL THEN RETURN NIL END;
  att := bex(Attr);
  WHILE (att # NIL) & (att.kind # kind) DO att := att.next END;
  RETURN att;
END attr;

PROCEDURE appAttr(VAR bex: pc.BEXT; att: Attr; kind: SHORTINT);
  VAR a: Attr;
BEGIN
  att.kind := kind;
  IF bex # NIL THEN
    a := bex(Attr);
    WHILE a # NIL DO ASSERT(a.kind#kind); a := a.next END;
    att.next := bex(Attr);
  ELSE att.next := NIL;
  END;
  bex := att;
END appAttr;

PROCEDURE delAttr*(VAR bex: pc.BEXT; kind: SHORTINT);
  VAR a0, a: Attr;
BEGIN
  IF bex # NIL THEN
    a0 := NIL;
    a := bex(Attr);
    LOOP
      IF a.kind = kind THEN
        IF a0 = NIL THEN bex := a.next ELSE a0.next := a.next END;
        EXIT
      END;
      a0 :=a; a := a.next;
      IF a = NIL THEN EXIT END;
    END;
  END;
END delAttr;

PROCEDURE appStructAttr*(str: pc.STRUCT; att: Attr; kind: SHORTINT);
BEGIN
  appAttr(str.ext, att, kind);
END appStructAttr;

PROCEDURE appObjAttr*(o: pc.OBJECT; att: Attr; kind: SHORTINT);
  -- VAR nm: NAME_EXT;
BEGIN
(* !!! с именами пока не работаю, добавлю если будет необходимость 
  IF (o.ext = NIL) & (kind # a_name) THEN
    NEW(nm);
    nm.kind := a_name; nm.list := NIL; nm.next := NIL;
    o.ext := nm;
  END;
*)  
  appAttr(o.ext, att, kind);
END appObjAttr;

(* ------- Attributes implementation ------- *)

CONST (** Attr.kind values *)
  a_self*        = 1;   (* Info -- type offset and ???!!! *)
  a_size*        = 2;   (* Size -- type size *)

TYPE
(* Allocation information. See notes for struct allocation  *)     
  Info *= POINTER TO InfoExt; (* a_self *)
  InfoExt *= RECORD (AttrRec)
    flInx*: LONGINT;  (** field index inside its host LLVM struct (real or pseudo-type) *)
    vType*: LONGINT;  (** id of the struct type generated for this variant, -1 for ordinary field   *)  
    fOffs*: LONGINT;  (** field offset in host struct (real record) *)
    host*:  Info;     (** link to parent variant if they are nested. NIL if parent is record *)
  END;

PROCEDURE app_info*(o: pc.OBJECT;
                 kind: SHORTINT;
                fld_x: LONGINT;
              var_tag: LONGINT;
                 offs: LONGINT;
                 host: Info);
  VAR inf: Info;
BEGIN
<* IF db_attrs THEN *>
  io.printf("app_info('%s', kind=%d, e_tag=%d, name=%d, offs=%d",
                  o.name^, akind,   e_tag,    name,    offs);
<* END *>
  NEW(inf);
  inf.kind  := kind;
  inf.flInx := fld_x;
  inf.vType := var_tag;
  inf.fOffs := offs;
  inf.host  := host;
  appObjAttr(o, inf, kind);
<* IF db_attrs THEN *>
  io.printf(")\n");
<* END *>
END app_info;

TYPE
  Size *= POINTER TO SizeRec;
  SizeRec *= RECORD (AttrRec)
    size*: LONGINT;
  END; 

END llAttrs.