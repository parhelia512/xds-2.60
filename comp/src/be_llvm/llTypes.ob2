(** Copyright (c) 1991,2023 Excelsior Ltd, Novosibirsk, Russia. All Rights Reserved. *)
(** XDS LLVM back-end: Reflection of the source types to LLVM ones                   *)
(** This module is sufficiently based on x86 back-end, esp. opDef.ob2                *)
MODULE llTypes; (* Hady 04-Mar-2023. *)

IMPORT
  pc   := pcK,
  at   := llAttrs,
  env  := xiEnv,
  tune := llTune;
  
PROCEDURE valid_name (name: pc.STRING) : BOOLEAN;
  VAR i,ln: LONGINT; ch: CHAR;
BEGIN
  IF name = NIL THEN RETURN FALSE END;
  ln := LEN(name^);
  i := 0;
  LOOP
    ch := name^[i];
    IF ch = 0X THEN RETURN (i#0) END;
    IF ((ch<'0') OR (ch>'9')) &  ((ch<'A') OR (ch>'Z')) &
       ((ch<'a') OR (ch>'z')) &  (ch#'_')
    THEN RETURN FALSE
    END;
    INC(i);
    IF i >= ln THEN RETURN TRUE END;
  END;
END valid_name;  
  
PROCEDURE err_type_size (t: pc.STRUCT);
VAR
  s  : ARRAY 256 OF CHAR;
  pos: pc.TPOS;
BEGIN
  IF (t.obj # NIL) & valid_name(t.obj.name) THEN
    COPY (t.obj.name^, s);
  ELSIF t.mode IN pc.ARRs THEN
    s := "an unnamed ARRAY inside some type definition";
  ELSIF t.mode = pc.ty_record THEN
    s := "an unnamed RECORD inside some type definition";
  ELSE
    s := "an unnamed type inside some type definition";
  END;
  pos := t.pos;
  IF pos.IsNull() AND (t.obj#NIL) THEN
    pos := t.obj.pos;
  END;
  env.errors.Error(pos,1014,s);
END err_type_size;

PROCEDURE mk_align(VAR offs: LONGINT; align: SHORTINT);
  VAR r: LONGINT;
BEGIN
  r := offs MOD align;
  IF r # 0 THEN INC(offs, align-r) END;
END mk_align;

PROCEDURE ^ record_size (t : pc.STRUCT; VAR size : LONGINT; VAR align : SHORTINT);

PROCEDURE bytes(t : pc.STRUCT; VAR sz : LONGINT; VAR align : SHORTINT);
  VAR n_align, t_align : SHORTINT;
BEGIN
  t_align := t.align;
  IF t_align = 0 THEN t_align := align END;
  CASE t.mode OF
  | pc.ty_shortcard, pc.ty_shortint :
      sz      := 1;
      n_align := 1;
  | pc.ty_cardinal, pc.ty_integer :
      sz      := 2;
      n_align := 2;
  | pc.ty_longcard, pc.ty_longint :
      sz      := 4;
      n_align := 4;
  | pc.ty_longlongint, pc.ty_longlongcard :
      sz      := 8;
      n_align := 8;
  | pc.ty_real :
      sz      := tune.real_sz;
      n_align := VAL(SHORTINT,tune.real_sz);
  | pc.ty_longreal :
      sz      := tune.longreal_sz;
      n_align := VAL(SHORTINT,tune.longreal_sz);
  | pc.ty_ld_real :
      sz      := tune.ld_real_sz;
      n_align := VAL(SHORTINT,tune.longreal_sz);   (* 8 bytes alignment is OK for all known platforms to the moment *)
  | pc.ty_complex :
      sz      := 2 * tune.real_sz;
      n_align := VAL(SHORTINT,tune.real_sz);
  | pc.ty_lcomplex :
      sz      := 2 * tune.longreal_sz;
      n_align := VAL(SHORTINT,tune.longreal_sz);
  | pc.ty_boolean, pc.ty_range, pc.ty_enum, pc.ty_protection :
      n_align := t_align;
      bytes(t.base, sz, n_align);
  | pc.ty_char :
      sz      := tune.char_sz;
      n_align := VAL(SHORTINT,tune.char_sz);
  | pc.ty_opaque, pc.ty_pointer, pc.ty_AA :
      sz      := tune.addr_sz;
      n_align := VAL(SHORTINT,tune.addr_sz);
  | pc.ty_set :
      IF t.inx#NIL THEN
        n_align := t_align;
        bytes(t.inx, sz, n_align);
      ELSIF t.len >= tune.BITSET_LEN THEN
        sz := ((t.len -1) DIV tune.BITSET_LEN +1)* tune.bitset_sz;
        n_align := VAL(SHORTINT,tune.bitset_sz);
      ELSIF t.len <= 8 THEN
        sz      := 1;
        n_align := 1;
      ELSIF t.len <=16 THEN
        sz      := 2;
        n_align := 2;
      ELSE
        sz      := 4;
        n_align := 4;
      END;
  | pc.ty_proctype :
      sz      := tune.proc_sz;
      n_align := VAL(SHORTINT,tune.proc_sz);
  | pc.ty_array :
      bytes(t.base, sz, align);
      IF sz > 0 THEN
        IF t.len <= MAX(LONGINT) DIV sz THEN
          sz := sz * t.len;
        ELSE
          sz := MAX(LONGINT)-1;
        END;
      END;
      RETURN
  | pc.ty_record :
      record_size(t, sz, align); RETURN
  | pc.ty_loc :
      sz      := 1;
      n_align := 1;
  | pc.ty_SS :
      sz      := tune.char_sz * t.len;
      n_align := VAL(SHORTINT,tune.char_sz);
  | pc.ty_process :
      sz      := tune.process_size;
      n_align := VAL(SHORTINT,tune.WORD_SIZE);
  | pc.ty_array_of :
      bytes(t.base, sz, align);
      sz := -1;
      RETURN
  ELSE
    sz    := 0; 
    align := 1;
    RETURN
  END;
  IF t_align > n_align THEN t_align := n_align END;
  IF t_align < align   THEN align   := t_align END;
END bytes;

PROCEDURE alloc_field(f : pc.OBJECT;
               VAR offs : LONGINT;
              VAR align : SHORTINT;
                field_x : LONGINT;
                   host : at.Info;
               cre_attr : BOOLEAN);
               
  VAR field_size: LONGINT; field_align: SHORTINT;
    
BEGIN
  field_align := align;
  bytes(f.type, field_size, field_align);
  ASSERT(field_size>=0);
  IF field_align > align THEN
    env.errors.Warning(f.pos, 324, f.name^, field_align, align);
  ELSIF field_align < align THEN
    align := field_align;
  END;
  IF offs <= MAX(LONGINT) - align THEN
    mk_align(offs, align);
  ELSE
    err_type_size(f.host);
  END;
  IF cre_attr THEN
    at.app_info(f, at.a_self, field_x, -1, offs, host);
    INCL(f.tags, at.otag_declared);
  END;
  IF offs <= MAX(LONGINT) - field_size THEN
    INC(offs, field_size);
  ELSE
    err_type_size(f.host)
  END;
END alloc_field;

PROCEDURE bit_field(f: pc.OBJECT; offs: LONGINT);
--  VAR wrd_offs: LONGINT;
BEGIN
(*
  IF (f.attr = NIL) OR (f.attr.l = NIL) THEN
    wrd_offs := offs;    (* how to take in account alignment ??? *)
  ELSE
    wrd_offs := BYTES_PER_WORD * f.attr.l.val.get_integer();
  END;
*)
(*
  at.app_info(f, at.a_self, ir.y_Nothing+at.BASE, at.ZEROInfExtName, NIL, offs);
  INCL(f.tags, at.otag_declared);
*)
  ASSERT(FALSE) (* !!Bit fields in records are not suported. Please report to developers *)  
END bit_field;

(* размещает список полей, перевычисляя смещение, и выдает выравнивание *)
PROCEDURE alloc_flist (f : pc.OBJECT;
                VAR offs : LONGINT;
               VAR align : SHORTINT;
                    host : at.Info;
                cre_attr : BOOLEAN);
  VAR n : pc.NODE;
    mx_size, l_size, start : LONGINT;
    field_x, variant_x, max_variant_x, var_x: LONGINT; 
    list_align, mx_align, l_align : SHORTINT;
    info: at.Info;
BEGIN
  list_align := 1;
  info:=NIL; field_x:=0; variant_x:=0;
  WHILE f # NIL DO
    IF f.mode = pc.ob_field THEN
      l_align := align;
      alloc_field(f, offs, l_align, field_x, host, cre_attr);
      IF l_align > list_align THEN list_align := l_align END;
    ELSIF f.mode = pc.ob_field_bts THEN
      IF cre_attr THEN bit_field(f, offs) END;
    ELSIF f.mode = pc.ob_header THEN
      IF f.val.obj # NIL THEN
        l_align := align;
        alloc_field(f.val.obj, offs, l_align, field_x, host, cre_attr);
        INC(field_x);   -- the variants header is a separate field, next field is for variants
        IF l_align > list_align THEN list_align := l_align END;
      END;
      mx_size := 0;
      mx_align := 1;
      
      n := f.val.l; var_x:=variant_x; max_variant_x:=variant_x;
      WHILE n # NIL DO
        ASSERT(n^.mode = pc.nd_node);
        l_size := 0; l_align := align; 
        alloc_flist(n.obj, l_size, l_align, NIL, FALSE);
        IF l_size > mx_size THEN mx_size := l_size; max_variant_x:=var_x; END;
        IF l_align > mx_align THEN mx_align := l_align END;
        n := n.next; INC(var_x);
      END;
      IF offs <= MAX(LONGINT) - mx_align THEN
        mk_align(offs, mx_align);
      ELSE
        err_type_size(f.host)
      END;
      
-- here we have to create field for a CASE with the static LLVM type of max_variant_x      

      IF cre_attr THEN
        NEW(info);
        info.host  := host;
        info.vType := variant_x;
        info.flInx := field_x;
        info.fOffs := 0;  (* it shall not be needed for variant pseudo-field itself *)       
        
        n  := f.val.l; 
        start := offs;
        WHILE n # NIL DO
          l_align := align;
          alloc_flist(n.obj, offs, l_align,  info, TRUE);
          offs := start;
          n := n.next; INC(variant_x);
        END;
      END;

      IF offs <= MAX(LONGINT) - mx_size THEN
        INC(offs, mx_size);
      ELSE
        err_type_size(f.host)
      END;
      IF mx_align > list_align THEN list_align := mx_align END;
    ELSE
      env.errors.Fault (f.pos, 962, f.mode);    ---  invalid object mode (%d)
    END;
    f := f.next; INC(field_x);
  END;
  align := list_align;
END alloc_flist;

PROCEDURE rec_size0 (t : pc.STRUCT; VAR size : LONGINT; VAR align : SHORTINT);
VAR
  base: pc.STRUCT;
  t_align, fl_align, b_align : SHORTINT;
BEGIN
  size := 0;
  b_align := -1;
  base := t.base;
  t_align := t.align;
  IF t_align = 0 THEN t_align := align END;
  IF base # NIL THEN
    b_align := t_align;
    rec_size0(base, size, b_align);
  END;
  fl_align := t_align;
  alloc_flist(t.prof, size, fl_align, NIL, FALSE);
  IF (base # NIL) & (b_align > fl_align) THEN
    align := b_align;
  ELSE
    align := fl_align;
  END;
END rec_size0;

PROCEDURE record_size (t : pc.STRUCT;
                VAR size : LONGINT; VAR align : SHORTINT);
BEGIN
  rec_size0(t, size, align);
  IF size = 0 THEN size := tune.empty_rec_size END;
  mk_align(size, align);
END record_size;

PROCEDURE type_size * (t: pc.STRUCT; bySize:=TRUE: BOOLEAN): LONGINT;
(** returns size of the given type *)
  VAR sz: LONGINT; align: SHORTINT;
BEGIN
  align := tune.default_alignment;
  bytes(t, sz, align);
  RETURN sz
END type_size;

PROCEDURE type_align * (t: pc.STRUCT; bySize:=TRUE: BOOLEAN): SHORTINT;
(** returns alignment of the given type *)
  VAR sz: LONGINT; t_align : SHORTINT;
BEGIN
  t_align := t.align;
  IF t_align = 0 THEN t_align := tune.default_alignment END;
  bytes(t, sz, t_align);
  RETURN t_align
END type_align;

(* ----- R e c o r d   D e c l a r a t i o n ---------- *)

PROCEDURE set_size(t: pc.STRUCT; size: LONGINT);
  VAR ext: at.Size;
BEGIN
  ASSERT(NOT (at.tmark_processed IN t.marks));
  NEW(ext); ext.size := size;
  at.appStructAttr(t, ext, at.a_size);
  INCL(t.marks, at.tmark_processed);
END set_size;

PROCEDURE get_align * (t: pc.STRUCT) : SHORTINT;
BEGIN
  IF t.align = 0 THEN
    RETURN tune.default_alignment
  ELSE
    RETURN t.align
  END;
END get_align;

PROCEDURE declare_record(t: pc.STRUCT);
  VAR offs : LONGINT;
   t_align : SHORTINT;
BEGIN
<* IF db_def THEN *>
  note("declare_record", t.obj);
  io.print(" ttags=%X, otags =%X\n", t.tags, t.obj.tags);
<* END *>

  IF NOT (at.tmark_processed IN t.marks) THEN
    offs := 0;
    IF t.base # NIL THEN
      declare_record(t.base);
      rec_size0(t.base, offs, t_align);
    END;
    t_align := get_align(t);
    alloc_flist(t.prof, offs, t_align, NIL, TRUE);
    IF offs = 0 THEN offs := tune.empty_rec_size END;
    set_size(t, offs);
  END;
END declare_record;

PROCEDURE type_definition(o: pc.OBJECT);
BEGIN
  IF o.type.mode = pc.ty_record THEN declare_record(o.type);
  ELSIF o.type.mode = pc.ty_array_of THEN        -- dynarr_definition(o.type);
  ELSE ASSERT(NOT (at.otag_declared IN o.tags)); -- type_declaration(o);
  END;
END type_definition;

PROCEDURE object_definition(o: pc.OBJECT);
(* окончательное описание объекта *)
BEGIN
  IF pc.OTAG_SET{at.otag_defining,at.otag_declaring}*o.tags#pc.OTAG_SET{} THEN
    env.errors.Error(o.pos, 1018);
    EXCL(o.tags,at.otag_undef);
    ASSERT(o.mode=pc.ob_type);
    RETURN;
  END;
<* IF db_def THEN *> note("object_definition", o); <* END *>
  INCL(o.tags,at.otag_defining);
  CASE o.mode OF
    |pc.ob_var,pc.ob_cons:
       (* var_definition(o); *)
    |pc.ob_proc,pc.ob_xproc,pc.ob_eproc:
       (* func_definition(o);  *)
    |pc.ob_cproc: 
       (* code_func_definition(o); -- A.K. - no actions currently *)
    |pc.ob_type: type_definition(o);
  ELSE
  END;
  EXCL(o.tags,at.otag_defining);
  EXCL(o.tags,at.otag_undef);
  INCL(o.tags,at.otag_declared);
END object_definition;

PROCEDURE makeOffsAttr(o: pc.OBJECT): at.Attr;
BEGIN
<* IF db_def THEN *>
  note("makeOffsAttr", o); io.print('     kind=%d\n', kind);
  IF at.otag_declared IN o.tags THEN io.print('declared\n') END;
<* END *>
  ASSERT( (o.mode # pc.ob_module)
         &(o.host.mode IN pc.TY_SET{pc.ty_enum,pc.ty_record})
         & NOT (o.mode IN pc.PROCs)); 
  IF NOT (at.otag_declared IN o.tags) THEN
    object_definition(o.host.obj);
    ASSERT(at.otag_declared IN o.tags);
  END;
  RETURN at.attr(o.ext, at.a_self)
(* -- этого не бывает ??
  | a_size        (* размер типа *)                    (* size_ext *)
  | a_prot        (* номер прототипа *)                (* prot_ext *)
  | a_type:                                            (* inf_ext  *)
--- *)
END makeOffsAttr;

PROCEDURE obj_offset * (o: pc.OBJECT): LONGINT;
(** returns offset of the given element inside the structure it belongs to. *) 
  VAR a: at.Attr;  inf: at.Info;
BEGIN
  a := at.attr(o.ext, at.a_self);
  IF a = NIL THEN a := makeOffsAttr(o) END;
  RETURN a(at.Info).fOffs;
END obj_offset;


BEGIN
END llTypes.