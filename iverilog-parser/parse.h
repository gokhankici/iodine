/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_VL_PARSE_HH_INCLUDED
# define YY_VL_PARSE_HH_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int VLdebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENTIFIER = 258,
    SYSTEM_IDENTIFIER = 259,
    STRING = 260,
    TIME_LITERAL = 261,
    TYPE_IDENTIFIER = 262,
    PACKAGE_IDENTIFIER = 263,
    DISCIPLINE_IDENTIFIER = 264,
    PATHPULSE_IDENTIFIER = 265,
    BASED_NUMBER = 266,
    DEC_NUMBER = 267,
    UNBASED_NUMBER = 268,
    REALTIME = 269,
    K_PLUS_EQ = 270,
    K_MINUS_EQ = 271,
    K_INCR = 272,
    K_DECR = 273,
    K_LE = 274,
    K_GE = 275,
    K_EG = 276,
    K_EQ = 277,
    K_NE = 278,
    K_CEQ = 279,
    K_CNE = 280,
    K_LP = 281,
    K_LS = 282,
    K_RS = 283,
    K_RSS = 284,
    K_SG = 285,
    K_CONTRIBUTE = 286,
    K_PO_POS = 287,
    K_PO_NEG = 288,
    K_POW = 289,
    K_PSTAR = 290,
    K_STARP = 291,
    K_DOTSTAR = 292,
    K_LOR = 293,
    K_LAND = 294,
    K_NAND = 295,
    K_NOR = 296,
    K_NXOR = 297,
    K_TRIGGER = 298,
    K_SCOPE_RES = 299,
    K_edge_descriptor = 300,
    K_always = 301,
    K_and = 302,
    K_assign = 303,
    K_begin = 304,
    K_buf = 305,
    K_bufif0 = 306,
    K_bufif1 = 307,
    K_case = 308,
    K_casex = 309,
    K_casez = 310,
    K_cmos = 311,
    K_deassign = 312,
    K_default = 313,
    K_defparam = 314,
    K_disable = 315,
    K_edge = 316,
    K_else = 317,
    K_end = 318,
    K_endcase = 319,
    K_endfunction = 320,
    K_endmodule = 321,
    K_endprimitive = 322,
    K_endspecify = 323,
    K_endtable = 324,
    K_endtask = 325,
    K_event = 326,
    K_for = 327,
    K_force = 328,
    K_forever = 329,
    K_fork = 330,
    K_function = 331,
    K_highz0 = 332,
    K_highz1 = 333,
    K_if = 334,
    K_ifnone = 335,
    K_initial = 336,
    K_inout = 337,
    K_input = 338,
    K_integer = 339,
    K_join = 340,
    K_large = 341,
    K_macromodule = 342,
    K_medium = 343,
    K_module = 344,
    K_nand = 345,
    K_negedge = 346,
    K_nmos = 347,
    K_nor = 348,
    K_not = 349,
    K_notif0 = 350,
    K_notif1 = 351,
    K_or = 352,
    K_output = 353,
    K_parameter = 354,
    K_pmos = 355,
    K_posedge = 356,
    K_primitive = 357,
    K_pull0 = 358,
    K_pull1 = 359,
    K_pulldown = 360,
    K_pullup = 361,
    K_rcmos = 362,
    K_real = 363,
    K_realtime = 364,
    K_reg = 365,
    K_release = 366,
    K_repeat = 367,
    K_rnmos = 368,
    K_rpmos = 369,
    K_rtran = 370,
    K_rtranif0 = 371,
    K_rtranif1 = 372,
    K_scalared = 373,
    K_small = 374,
    K_specify = 375,
    K_specparam = 376,
    K_strong0 = 377,
    K_strong1 = 378,
    K_supply0 = 379,
    K_supply1 = 380,
    K_table = 381,
    K_task = 382,
    K_time = 383,
    K_tran = 384,
    K_tranif0 = 385,
    K_tranif1 = 386,
    K_tri = 387,
    K_tri0 = 388,
    K_tri1 = 389,
    K_triand = 390,
    K_trior = 391,
    K_trireg = 392,
    K_vectored = 393,
    K_wait = 394,
    K_wand = 395,
    K_weak0 = 396,
    K_weak1 = 397,
    K_while = 398,
    K_wire = 399,
    K_wor = 400,
    K_xnor = 401,
    K_xor = 402,
    K_Shold = 403,
    K_Snochange = 404,
    K_Speriod = 405,
    K_Srecovery = 406,
    K_Ssetup = 407,
    K_Ssetuphold = 408,
    K_Sskew = 409,
    K_Swidth = 410,
    KK_attribute = 411,
    K_bool = 412,
    K_logic = 413,
    K_automatic = 414,
    K_endgenerate = 415,
    K_generate = 416,
    K_genvar = 417,
    K_localparam = 418,
    K_noshowcancelled = 419,
    K_pulsestyle_onevent = 420,
    K_pulsestyle_ondetect = 421,
    K_showcancelled = 422,
    K_signed = 423,
    K_unsigned = 424,
    K_Sfullskew = 425,
    K_Srecrem = 426,
    K_Sremoval = 427,
    K_Stimeskew = 428,
    K_cell = 429,
    K_config = 430,
    K_design = 431,
    K_endconfig = 432,
    K_incdir = 433,
    K_include = 434,
    K_instance = 435,
    K_liblist = 436,
    K_library = 437,
    K_use = 438,
    K_wone = 439,
    K_uwire = 440,
    K_alias = 441,
    K_always_comb = 442,
    K_always_ff = 443,
    K_always_latch = 444,
    K_assert = 445,
    K_assume = 446,
    K_before = 447,
    K_bind = 448,
    K_bins = 449,
    K_binsof = 450,
    K_bit = 451,
    K_break = 452,
    K_byte = 453,
    K_chandle = 454,
    K_class = 455,
    K_clocking = 456,
    K_const = 457,
    K_constraint = 458,
    K_context = 459,
    K_continue = 460,
    K_cover = 461,
    K_covergroup = 462,
    K_coverpoint = 463,
    K_cross = 464,
    K_dist = 465,
    K_do = 466,
    K_endclass = 467,
    K_endclocking = 468,
    K_endgroup = 469,
    K_endinterface = 470,
    K_endpackage = 471,
    K_endprogram = 472,
    K_endproperty = 473,
    K_endsequence = 474,
    K_enum = 475,
    K_expect = 476,
    K_export = 477,
    K_extends = 478,
    K_extern = 479,
    K_final = 480,
    K_first_match = 481,
    K_foreach = 482,
    K_forkjoin = 483,
    K_iff = 484,
    K_ignore_bins = 485,
    K_illegal_bins = 486,
    K_import = 487,
    K_inside = 488,
    K_int = 489,
    K_interface = 490,
    K_intersect = 491,
    K_join_any = 492,
    K_join_none = 493,
    K_local = 494,
    K_longint = 495,
    K_matches = 496,
    K_modport = 497,
    K_new = 498,
    K_null = 499,
    K_package = 500,
    K_packed = 501,
    K_priority = 502,
    K_program = 503,
    K_property = 504,
    K_protected = 505,
    K_pure = 506,
    K_rand = 507,
    K_randc = 508,
    K_randcase = 509,
    K_randsequence = 510,
    K_ref = 511,
    K_return = 512,
    K_sequence = 513,
    K_shortint = 514,
    K_shortreal = 515,
    K_solve = 516,
    K_static = 517,
    K_string = 518,
    K_struct = 519,
    K_super = 520,
    K_tagged = 521,
    K_this = 522,
    K_throughout = 523,
    K_timeprecision = 524,
    K_timeunit = 525,
    K_type = 526,
    K_typedef = 527,
    K_union = 528,
    K_unique = 529,
    K_var = 530,
    K_virtual = 531,
    K_void = 532,
    K_wait_order = 533,
    K_wildcard = 534,
    K_with = 535,
    K_within = 536,
    K_timeprecision_check = 537,
    K_timeunit_check = 538,
    K_accept_on = 539,
    K_checker = 540,
    K_endchecker = 541,
    K_eventually = 542,
    K_global = 543,
    K_implies = 544,
    K_let = 545,
    K_nexttime = 546,
    K_reject_on = 547,
    K_restrict = 548,
    K_s_always = 549,
    K_s_eventually = 550,
    K_s_nexttime = 551,
    K_s_until = 552,
    K_s_until_with = 553,
    K_strong = 554,
    K_sync_accept_on = 555,
    K_sync_reject_on = 556,
    K_unique0 = 557,
    K_until = 558,
    K_until_with = 559,
    K_untyped = 560,
    K_weak = 561,
    K_implements = 562,
    K_interconnect = 563,
    K_nettype = 564,
    K_soft = 565,
    K_above = 566,
    K_abs = 567,
    K_absdelay = 568,
    K_abstol = 569,
    K_access = 570,
    K_acos = 571,
    K_acosh = 572,
    K_ac_stim = 573,
    K_aliasparam = 574,
    K_analog = 575,
    K_analysis = 576,
    K_asin = 577,
    K_asinh = 578,
    K_atan = 579,
    K_atan2 = 580,
    K_atanh = 581,
    K_branch = 582,
    K_ceil = 583,
    K_connect = 584,
    K_connectmodule = 585,
    K_connectrules = 586,
    K_continuous = 587,
    K_cos = 588,
    K_cosh = 589,
    K_ddt = 590,
    K_ddt_nature = 591,
    K_ddx = 592,
    K_discipline = 593,
    K_discrete = 594,
    K_domain = 595,
    K_driver_update = 596,
    K_endconnectrules = 597,
    K_enddiscipline = 598,
    K_endnature = 599,
    K_endparamset = 600,
    K_exclude = 601,
    K_exp = 602,
    K_final_step = 603,
    K_flicker_noise = 604,
    K_floor = 605,
    K_flow = 606,
    K_from = 607,
    K_ground = 608,
    K_hypot = 609,
    K_idt = 610,
    K_idtmod = 611,
    K_idt_nature = 612,
    K_inf = 613,
    K_initial_step = 614,
    K_laplace_nd = 615,
    K_laplace_np = 616,
    K_laplace_zd = 617,
    K_laplace_zp = 618,
    K_last_crossing = 619,
    K_limexp = 620,
    K_ln = 621,
    K_log = 622,
    K_max = 623,
    K_merged = 624,
    K_min = 625,
    K_nature = 626,
    K_net_resolution = 627,
    K_noise_table = 628,
    K_paramset = 629,
    K_potential = 630,
    K_pow = 631,
    K_resolveto = 632,
    K_sin = 633,
    K_sinh = 634,
    K_slew = 635,
    K_split = 636,
    K_sqrt = 637,
    K_tan = 638,
    K_tanh = 639,
    K_timer = 640,
    K_transition = 641,
    K_units = 642,
    K_white_noise = 643,
    K_wreal = 644,
    K_zi_nd = 645,
    K_zi_np = 646,
    K_zi_zd = 647,
    K_zi_zp = 648,
    K_TAND = 649,
    K_MUL_EQ = 650,
    K_DIV_EQ = 651,
    K_MOD_EQ = 652,
    K_AND_EQ = 653,
    K_OR_EQ = 654,
    K_XOR_EQ = 655,
    K_LS_EQ = 656,
    K_RS_EQ = 657,
    K_RSS_EQ = 658,
    UNARY_PREC = 659,
    less_than_K_else = 660
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 366 "parse.y" /* yacc.c:1909  */

      bool flag;

      char letter;
      int  int_val;

	/* text items are C strings allocated by the lexor using
	   strdup. They can be put into lists with the texts type. */
      char*text;
      list<perm_string>*perm_strings;

      list<pform_port_t>*port_list;

      vector<pform_tf_port_t>* tf_ports;

      pform_name_t*pform_name;

      ivl_discipline_t discipline;

      hname_t*hier;

      list<string>*strings;

      struct str_pair_t drive;

      PCase::Item*citem;
      svector<PCase::Item*>*citems;

      lgate*gate;
      svector<lgate>*gates;

      Module::port_t *mport;
      LexicalScope::range_t* value_range;
      vector<Module::port_t*>*mports;

      named_number_t* named_number;
      list<named_number_t>* named_numbers;

      named_pexpr_t*named_pexpr;
      list<named_pexpr_t>*named_pexprs;
      struct parmvalue_t*parmvalue;
      list<pform_range_t>*ranges;

      PExpr*expr;
      list<PExpr*>*exprs;

      svector<PEEvent*>*event_expr;

      NetNet::Type nettype;
      PGBuiltin::Type gatetype;
      NetNet::PortType porttype;
      ivl_variable_type_t vartype;
      PBlock::BL_TYPE join_keyword;

      PWire*wire;
      vector<PWire*>*wires;

      PEventStatement*event_statement;
      Statement*statement;
      vector<Statement*>*statement_list;

      net_decl_assign_t*net_decl_assign;
      enum_type_t*enum_type;

      decl_assignment_t*decl_assignment;
      list<decl_assignment_t*>*decl_assignments;

      struct_member_t*struct_member;
      list<struct_member_t*>*struct_members;
      struct_type_t*struct_type;

      data_type_t*data_type;
      class_type_t*class_type;
      real_type_t::type_t real_type;
      property_qualifier_t property_qualifier;
      PPackage*package;

      struct {
	    char*text;
	    data_type_t*type;
      } type_identifier;

      struct {
	    data_type_t*type;
	    list<PExpr*>*exprs;
      } class_declaration_extends;

      verinum* number;

      verireal* realtime;

      PSpecPath* specpath;
      list<index_component_t> *dimensions;

      LexicalScope::lifetime_t lifetime;

#line 557 "parse.hh" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE VLlval;
extern YYLTYPE VLlloc;
int VLparse (void);

#endif /* !YY_VL_PARSE_HH_INCLUDED  */
