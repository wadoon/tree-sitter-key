let keywords = ["any", "any_bit"];

the_grammar = {
  name: "key",
  conflicts: ($) => [
    [$.comparison_term],
    [$.weak_arith_term],
    [$.comparison_term],
    [$.comparison_term],
    [$.strong_arith_term_1],
    [$.strong_arith_term_2],
    [$.primitive_labeled_term],
  ],
  rules: {
    source_file: ($) =>
      seq(
        repeat($.DOC_COMMENT),
        repeat($.decls),
         optional($.problem), 
         optional($.proof)
      ),
    decls: ($) =>
      choice(
        $.profile,
        field("pref", $.preferences),
        $.bootClassPath,
        field("stlist", $.classPaths),
        field("string", $.javaSource),
        $.one_include_statement,
        $.options_choice,
        $.option_decls,
        $.sort_decls,
        $.prog_var_decls,
        $.schema_var_decls,
        $.pred_decls,
        $.func_decls,
        $.transform_decls,
        $.ruleset_decls,
        $.contracts,
        $.invariants,
        $.rulesOrAxioms
      ),
    problemStatement: $ =>  seq($.PROBLEM, $.LBRACE, field("a", $.term), $.RBRACE),
      chooseContract: $ =>       seq(
        $.CHOOSECONTRACT,
        optional(seq(field("chooseContract", $.string_value), $.SEMI))
      ),
proofObl: $=>seq(
  $.PROOFOBLIGATION,
  optional(seq(field("proofObligation", $.string_value), $.SEMI))
),
    problem: ($) =>
      seq(
        choice(
          $.problemStatement,
          $.chooseContract,
          $.proofObl,
        ),
        optional($.proofScript)
      ),
    one_include_statement: ($) =>
      seq(
        choice($.INCLUDE, $.INCLUDELDTS),
        $.one_include,
        repeat(seq($.COMMA, $.one_include)),
        $.SEMI
      ),
    one_include: ($) =>
      choice(field("absfile", $.IDENT), field("relfile", $.string_value)),
    options_choice: ($) =>
      seq(
        $.WITHOPTIONS,
        $.activated_choice,
        repeat(seq($.COMMA, $.activated_choice)),
        $.SEMI
      ),
    activated_choice: ($) =>
      seq(field("cat", $.IDENT), $.COLON, field("choice_", $.IDENT)),
    option_decls: ($) =>
      seq($.OPTIONSDECL, $.LBRACE, repeat(seq($.choice, $.SEMI)), $.RBRACE),
    choice: ($) =>
      seq(
        repeat(field("maindoc", $.DOC_COMMENT)),
        field("category", $.IDENT),
        optional(
          seq(
            $.COLON,
            $.LBRACE,
            $.optionDecl,
            repeat(seq($.COMMA, $.optionDecl)),
            $.RBRACE
          )
        )
      ),
    optionDecl: ($) =>
      seq(
        optional(field("doc", $.DOC_COMMENT)),
        field("choice_option", $.IDENT)
      ),
    sort_decls: ($) =>
      seq($.SORTS, $.LBRACE, repeat($.one_sort_decl), $.RBRACE),
    one_sort_decl: ($) =>
      seq(
        optional(field("doc", $.DOC_COMMENT)),
        choice(
          seq(
            $.GENERIC,
            field("sortIds", $.simple_ident_dots_comma_list),
            optional(seq($.ONEOF, field("sortOneOf", $.oneof_sorts))),
            optional(seq($.EXTENDS, field("sortExt", $.extends_sorts))),
            $.SEMI
          ),
          seq(
            $.PROXY,
            field("sortIds", $.simple_ident_dots_comma_list),
            optional(seq($.EXTENDS, field("sortExt", $.extends_sorts))),
            $.SEMI
          ),
          seq(
            optional($.ABSTRACT),
            field("sortIds", $.simple_ident_dots_comma_list),
            optional(seq($.EXTENDS, field("sortExt", $.extends_sorts))),
            $.SEMI
          )
        )
      ),
    simple_ident_dots: ($) =>
      prec(
        10,
        choice(
          seq($.simple_ident, repeat(seq($.DOT, $.simple_ident)))
          //, $.INT_LITERAL
        )
      ),
    simple_ident_dots_comma_list: ($) =>
      seq($.simple_ident_dots, repeat(seq($.COMMA, $.simple_ident_dots))),
    extends_sorts: ($) => seq($.sortId, repeat(seq($.COMMA, $.sortId))),
    oneof_sorts: ($) =>
      seq(
        $.LBRACE,
        field("s", $.sortId),
        repeat(seq($.COMMA, field("s", $.sortId))),
        $.RBRACE
      ),
    keyjavatype: ($) =>
      seq(field("type", $.simple_ident_dots), repeat($.EMPTYBRACKETS)),
    prog_var_decls: ($) =>
      seq(
        $.PROGRAMVARIABLES,
        $.LBRACE,
        repeat(
          seq(
            field("kjt", $.keyjavatype),
            field("var_names", $.simple_ident_comma_list),
            $.SEMI
          )
        ),
        $.RBRACE
      ),
    simple_ident: ($) => field("id", $.IDENT),
    simple_ident_comma_list: ($) =>
      seq(
        field("id", $.simple_ident),
        repeat(seq($.COMMA, field("id", $.simple_ident)))
      ),
    schema_var_decls: ($) =>
      seq(
        $.SCHEMAVARIABLES,
        $.LBRACE,
        repeat(seq($.one_schema_var_decl, $.SEMI)),
        $.RBRACE
      ),
    one_schema_var_decl: ($) =>
      choice(
        seq($.MODALOPERATOR, $.one_schema_modal_op_decl),
        seq(
          $.PROGRAM,
          optional($.schema_modifiers),
          field("id", $.simple_ident),
          optional(
            seq(
              $.LBRACKET,
              field("nameString", $.simple_ident),
              $.EQUALS,
              field("parameter", $.simple_ident_dots),
              $.RBRACKET
            )
          ),
          field("ids", $.simple_ident_comma_list)
        ),
        seq(
          $.FORMULA,
          optional($.schema_modifiers),
          field("ids", $.simple_ident_comma_list)
        ),
        seq(
          $.TERMLABEL,
          optional($.schema_modifiers),
          field("ids", $.simple_ident_comma_list)
        ),
        seq(
          $.UPDATE,
          optional($.schema_modifiers),
          field("ids", $.simple_ident_comma_list)
        ),
        seq(
          $.SKOLEMFORMULA,
          optional($.schema_modifiers),
          field("ids", $.simple_ident_comma_list)
        ),
        seq(
          choice($.TERM, choice($.VARIABLES, $.VARIABLE), $.SKOLEMTERM),
          optional($.schema_modifiers),
          field("s", $.sortId),
          field("ids", $.simple_ident_comma_list)
        )
      ),
    schema_modifiers: ($) =>
      seq($.LBRACKET, field("opts", $.simple_ident_comma_list), $.RBRACKET),
    one_schema_modal_op_decl: ($) =>
      seq(
        optional(seq($.LPAREN, field("sort", $.sortId), $.RPAREN)),
        $.LBRACE,
        field("ids", $.simple_ident_comma_list),
        $.RBRACE,
        field("id", $.simple_ident)
      ),
    pred_decl: ($) =>
      seq(
        optional(field("doc", $.DOC_COMMENT)),
        field("pred_name", $.funcpred_name),
        optional(field("whereToBind", $.where_to_bind)),
        field("argSorts", optional($.arg_sorts)),
        $.SEMI
      ),
    pred_decls: ($) =>
      seq($.PREDICATES, $.LBRACE, repeat($.pred_decl), $.RBRACE),
    func_decl: ($) =>
      seq(
        optional(field("doc", $.DOC_COMMENT)),
        optional($.UNIQUE),
        field("retSort", $.sortId),
        field("func_name", $.funcpred_name),
        optional(field("whereToBind", $.where_to_bind)),
        field("argSorts", optional($.arg_sorts)),
        $.SEMI
      ),
    func_decls: ($) =>
      seq($.FUNCTIONS, $.LBRACE, repeat($.func_decl), $.RBRACE),
    arg_sorts_or_formula: ($) =>
      seq(
        $.LPAREN,
        $.arg_sorts_or_formula_helper,
        repeat(seq($.COMMA, $.arg_sorts_or_formula_helper)),
        $.RPAREN
      ),
    arg_sorts_or_formula_helper: ($) => choice($.sortId, $.FORMULA),
    transform_decl: ($) =>
      seq(
        optional(field("doc", $.DOC_COMMENT)),
        choice(field("retSort", $.sortId), $.FORMULA),
        field("trans_name", $.funcpred_name),
        field("argSorts", optional($.arg_sorts_or_formula)),
        $.SEMI
      ),
    transform_decls: ($) =>
      seq($.TRANSFORMERS, $.LBRACE, repeat($.transform_decl), $.RBRACE),
    arrayopid: ($) =>
      seq(
        $.EMPTYBRACKETS,
        $.LPAREN,
        field("componentType", $.keyjavatype),
        $.RPAREN
      ),
    arg_sorts: ($) =>
      seq($.LPAREN, $.sortId, repeat(seq($.COMMA, $.sortId)), $.RPAREN),
    where_to_bind: ($) =>
      seq(
        $.LBRACE,
        field("b", choice($.TRUE, $.FALSE)),
        repeat(seq($.COMMA, field("b", choice($.TRUE, $.FALSE)))),
        $.RBRACE
      ),
    ruleset_decls: ($) =>
      seq(
        $.HEURISTICSDECL,
        $.LBRACE,
        repeat(
          seq(
            optional(field("doc", $.DOC_COMMENT)),
            field("id", $.simple_ident),
            $.SEMI
          )
        ),
        $.RBRACE
      ),
    sortId: ($) =>
      seq(field("id", $.simple_ident_dots), repeat($.EMPTYBRACKETS)),
    id_declaration: ($) =>
      seq(field("id", $.IDENT), optional(seq($.COLON, field("s", $.sortId)))),
    funcpred_name: ($) =>
      seq(
        optional(seq($.sortId, $.DOUBLECOLON)),
        field("name", $.simple_ident_dots)
      ),
    boolean_literal: ($) => choice($.TRUE, $.FALSE),
    literals: ($) =>
      choice(
        $.boolean_literal,
        $.char_literal,
        $.integer,
        $.floatnum,
        $.string_literal
      ),
    term: ($) => $.parallel_term,
    parallel_term: ($) =>
      seq(
        field("a", $.elementary_update_term),
        repeat(seq($.PARALLEL, field("b", $.elementary_update_term)))
      ),
    elementary_update_term: ($) =>
      prec.left(
        seq(
          field("a", $.equivalence_term),
          optional(seq($.ASSIGN, field("b", $.equivalence_term)))
        )
      ),
    equivalence_term: ($) =>
      seq(
        field("a", $.implication_term),
        repeat(seq($.EQV, field("b", $.implication_term)))
      ),
    implication_term: ($) =>
      seq(
        field("a", $.disjunction_term),
        optional(seq($.IMP, field("b", $.implication_term)))
      ),
    disjunction_term: ($) =>
      seq(
        field("a", $.conjunction_term),
        repeat(seq($.OR, field("b", $.conjunction_term)))
      ),
    conjunction_term: ($) =>
      seq(field("a", $.term60), repeat(seq($.AND, field("b", $.term60)))),
    term60: ($) => choice($.unary_formula, $.equality_term),
    unary_formula: ($) =>
      choice(
        field("negation_term", seq($.NOT, field("sub", $.term60))),
        field(
          "quantifierterm",
          seq(
            choice($.FORALL, $.EXISTS),
            $.bound_variables,
            field("sub", $.term60)
          )
        ),
        field("modality_term", seq($.MODALITY, field("sub", $.term60)))
      ),
    equality_term: ($) =>
      prec.left(
        seq(
          field("a", $.comparison_term),
          optional(
            seq(choice($.NOT_EQUALS, $.EQUALS), field("b", $.comparison_term))
          )
        )
      ),
    comparison_term: ($) =>
      seq(
        field("a", $.weak_arith_term),
        optional(
          seq(
            choice($.LESS, $.LESSEQUAL, $.GREATER, $.GREATEREQUAL),
            field("b", $.weak_arith_term)
          )
        )
      ),
    weak_arith_term: ($) =>
      seq(
        field("a", $.strong_arith_term_1),
        repeat(
          seq(
            field("op", choice($.PLUS, $.MINUS)),
            field("b", $.strong_arith_term_1)
          )
        )
      ),
    strong_arith_term_1: ($) =>
      seq(
        field("a", $.strong_arith_term_2),
        repeat(seq($.STAR, field("b", $.strong_arith_term_2)))
      ),
    strong_arith_term_2: ($) =>
      seq(
        field("a", $.atom_prefix),
        optional(
          seq(choice($.PERCENT, $.SLASH), field("b", $.strong_arith_term_2))
        )
      ),
    update_term: ($) =>
      seq(
        seq($.LBRACE, field("u", $.term), $.RBRACE),
        choice($.atom_prefix, $.unary_formula)
      ),
    substitution_term: ($) =>
      seq(
        $.LBRACE,
        $.SUBST,
        field("bv", $.one_bound_variable),
        $.SEMI,
        field("replacement", $.comparison_term),
        $.RBRACE,
        choice($.atom_prefix, $.unary_formula)
      ),
    cast_term: ($) =>
      seq(
        seq($.LPAREN, field("sort", $.sortId), $.RPAREN),
        field("sub", $.atom_prefix)
      ),
    unary_minus_term: ($) => seq($.MINUS, field("sub", $.atom_prefix)),
    atom_prefix: ($) =>
      choice(
        $.update_term,
        $.substitution_term,
        $.locset_term,
        $.cast_term,
        $.unary_minus_term,
        $.bracket_term
      ),
    bracket_term: ($) =>
      prec.left(
        seq(
          $.primitive_labeled_term,
          repeat($.bracket_suffix_heap),
          repeat($.attribute)
        )
      ),
    bracket_suffix_heap: ($) =>
      seq($.brace_suffix, optional(seq($.AT, field("heap", $.bracket_term)))),
    brace_suffix: ($) =>
      prec(
        10,
        choice(
          field(
            "bracket_access_heap_update",
            seq(
              $.LBRACKET,
              field("target", $.term),
              $.ASSIGN,
              field("val", $.term),
              $.RBRACKET
            )
          ),
          field(
            "bracket_access_heap_term",
            seq(
              $.LBRACKET,
              field("id", $.simple_ident),
              field("args", $.argument_list),
              $.RBRACKET
            )
          ),
          field("bracket_access_star", seq($.LBRACKET, $.STAR, $.RBRACKET)),
          field(
            "bracket_access_indexrange",
            seq(
              $.LBRACKET,
              field("indexTerm", $.term),
              optional(seq($.DOTRANGE, field("rangeTo", $.term))),
              $.RBRACKET
            )
          )
        )
      ),
    primitive_labeled_term: ($) =>
      seq(
        $.primitive_term,
        optional(seq($.LGUILLEMETS, field("labels", $.label), $.RGUILLEMETS))
      ),
    termParen: ($) =>
      prec.left(seq($.LPAREN, $.term, $.RPAREN, repeat($.attribute))),
    abbreviation: ($) => seq($.AT, field("name", $.simple_ident)),
    primitive_term: ($) =>
      choice(
        $.termParen,
        $.ifThenElseTerm,
        $.ifExThenElseTerm,
        $.abbreviation,
        $.accessterm,
        $.literals
      ),
    accessterm: ($) =>
      prec.right(
        seq(
          optional(seq($.sortId, $.DOUBLECOLON)),
          field("firstName", $.simple_ident),
          optional($.call),
          repeat($.attribute)
        )
      ),
    attribute: ($) =>
      prec(
        10,
        choice(
          field("attribute_star", seq($.DOT, $.STAR)),
          field(
            "attribute_simple",
            seq(
              $.DOT,
              field("id", $.simple_ident),
              optional($.call),
              optional(seq($.AT, field("heap", $.bracket_term)))
            )
          ),
          field(
            "attribute_complex",
            seq(
              $.DOT,
              $.LPAREN,
              field("sort", $.sortId),
              $.DOUBLECOLON,
              field("id", $.simple_ident),
              $.RPAREN,
              optional($.call),
              optional(seq($.AT, field("heap", $.bracket_term)))
            )
          )
        )
      ),
    call: ($) =>
      seq(
        optional(
          seq($.LBRACE, field("boundVars", $.bound_variables), $.RBRACE)
        ),
        $.argument_list
      ),
    label: ($) =>
      seq(
        field("l", $.single_label),
        repeat(seq($.COMMA, field("l", $.single_label)))
      ),
    single_label: ($) =>
      seq(
        choice(field("name", $.IDENT), field("star", $.STAR)),
        optional(
          seq(
            $.LPAREN,
            optional(seq($.string_value, repeat(seq($.COMMA, $.string_value)))),
            $.RPAREN
          )
        )
      ),
    location_term: ($) =>
      seq(
        $.LPAREN,
        field("obj", $.equivalence_term),
        $.COMMA,
        field("field", $.equivalence_term),
        $.RPAREN
      ),
    ifThenElseTerm: ($) =>
      seq(
        $.IF,
        $.LPAREN,
        field("condF", $.term),
        $.RPAREN,
        $.THEN,
        $.LPAREN,
        field("thenT", $.term),
        $.RPAREN,
        $.ELSE,
        $.LPAREN,
        field("elseT", $.term),
        $.RPAREN
      ),
    ifExThenElseTerm: ($) =>
      seq(
        $.IFEX,
        field("exVars", $.bound_variables),
        $.LPAREN,
        field("condF", $.term),
        $.RPAREN,
        $.THEN,
        $.LPAREN,
        field("thenT", $.term),
        $.RPAREN,
        $.ELSE,
        $.LPAREN,
        field("elseT", $.term),
        $.RPAREN
      ),
    locset_term: ($) =>
      seq(
        $.LBRACE,
        optional(
          seq(
            field("l", $.location_term),
            repeat(seq($.COMMA, field("l", $.location_term)))
          )
        ),
        $.RBRACE
      ),
    bound_variables: ($) =>
      seq(
        field("var", $.one_bound_variable),
        repeat(seq($.COMMA, field("var", $.one_bound_variable))),
        $.SEMI
      ),
    one_bound_variable: ($) =>
      seq(optional(field("s", $.sortId)), field("id", $.simple_ident)),
    argument_list: ($) =>
      seq(
        $.LPAREN,
        optional(seq($.term, repeat(seq($.COMMA, $.term)))),
        $.RPAREN
      ),
    integer: ($) => $.INT_LITERAL,
    //seq(optional($.MINUS),$.INT_LITERAL),
    /*
      seq(
        choice(
          ,
          $.HEX_LITERAL, $.BIN_LITERAL)
      ),*/
    floatnum: ($) =>
      choice(
        field("floatLiteral", seq($.FLOAT_LITERAL)),
        field("doubleLiteral", seq($.DOUBLE_LITERAL)),
        field("realLiteral", seq($.REAL_LITERAL))
      ),
    char_literal: ($) => $.CHAR_LITERAL,
    varId: ($) => field("id", $.IDENT),
    varIds: ($) => field("ids", $.simple_ident_comma_list),
    triggers: ($) =>
      seq(
        $.TRIGGER,
        $.LBRACE,
        field("id", $.simple_ident),
        $.RBRACE,
        field("t", $.term),
        optional(
          seq(
            $.AVOID,
            field("avoidCond", $.term),
            repeat(seq($.COMMA, field("avoidCond", $.term)))
          )
        ),
        $.SEMI
      ),
    taclet: ($) =>
      seq(
        optional(field("doc", $.DOC_COMMENT)),
        optional($.LEMMA),
        field("name", $.IDENT),
        optional(field("choices_", $.option_list)),
        $.LBRACE,
        choice(
          field("form", $.term),
          seq(
            repeat(seq($.SCHEMAVAR, $.one_schema_var_decl, $.SEMI)),
            optional(seq($.ASSUMES, $.LPAREN, field("ifSeq", $.seq), $.RPAREN)),
            optional(
              seq(
                $.FIND,
                $.LPAREN,
                field("find", $.termorseq),
                $.RPAREN,
                repeat(
                  choice(
                    $.SAMEUPDATELEVEL,
                    $.INSEQUENTSTATE,
                    $.ANTECEDENTPOLARITY,
                    $.SUCCEDENTPOLARITY
                  )
                )
              )
            ),
            repeat(seq($.VARCOND, $.LPAREN, $.varexplist, $.RPAREN)),
            $.goalspecs,
            optional($.modifiers)
          )
        ),
        $.RBRACE
      ),
    modifiers: ($) =>
      repeat1(
        choice(
          field("rs", $.rulesets),
          $.NONINTERACTIVE,
          seq($.DISPLAYNAME, field("dname", $.string_value)),
          seq($.HELPTEXT, field("htext", $.string_value)),
          $.triggers
        )
      ),
    seq: ($) =>
      seq(
        field("ant", optional($.semisequent)),
        $.SEQARROW,
        field("suc", optional($.semisequent))
      ),
    termorseq: ($) =>
      choice(
        seq(
          field("head", $.term),
          optional(
            choice(
              seq($.COMMA, field("s", $.seq)),
              seq($.SEQARROW, field("ss", optional($.semisequent)))
            )
          )
        ),
        seq($.SEQARROW, field("ss", optional($.semisequent)))
      ),
    semisequent: ($) =>
      seq(
        field("head", $.term),
        optional(seq($.COMMA, field("ss", $.semisequent)))
      ),
    varexplist: ($) => seq($.varexp, repeat(seq($.COMMA, $.varexp))),
    varexp: ($) =>
      seq(
        optional(field("negate", $.NOT_)),
        $.varexpId,
        optional(
          seq(
            $.LBRACKET,
            field("parameter", $.IDENT),
            repeat(seq($.COMMA, field("parameter", $.IDENT))),
            $.RBRACKET
          )
        ),
        optional(
          seq(
            $.LPAREN,
            $.varexp_argument,
            repeat(seq($.COMMA, $.varexp_argument)),
            $.RPAREN
          )
        )
      ),
    varexpId: ($) =>
      choice(
        $.APPLY_UPDATE_ON_RIGID,
        $.SAME_OBSERVER,
        $.DROP_EFFECTLESS_ELEMENTARIES,
        $.DROP_EFFECTLESS_STORES,
        $.DIFFERENTFIELDS,
        $.SIMPLIFY_IF_THEN_ELSE_UPDATE,
        $.CONTAINS_ASSIGNMENT,
        $.ISENUMTYPE,
        $.ISTHISREFERENCE,
        $.STATICMETHODREFERENCE,
        $.ISREFERENCEARRAY,
        $.ISARRAY,
        $.ISARRAYLENGTH,
        $.IS_ABSTRACT_OR_INTERFACE,
        $.ENUM_CONST,
        $.FINAL,
        $.STATIC,
        $.ISLOCALVARIABLE,
        $.ISOBSERVER,
        $.DIFFERENT,
        $.METADISJOINT,
        $.EQUAL_UNIQUE,
        $.FREELABELIN,
        $.ISCONSTANT,
        $.HASLABEL,
        $.ISSTATICFIELD,
        $.HASSUBFORMULAS,
        $.FIELDTYPE,
        $.NEW,
        $.NEW_TYPE_OF,
        $.NEW_DEPENDING_ON,
        $.HAS_ELEMENTARY_SORT,
        $.SAME,
        $.ISSUBTYPE,
        seq($.STRICT, $.ISSUBTYPE),
        $.DISJOINTMODULONULL,
        $.NOTFREEIN,
        $.HASSORT,
        $.NEWLABEL,
        $.ISREFERENCE,
        $.MAXEXPANDMETHOD,
        $.STORE_TERM_IN,
        $.STORE_STMT_IN,
        $.HAS_INVARIANT,
        $.GET_INVARIANT,
        $.GET_FREE_INVARIANT,
        $.GET_VARIANT,
        $.IS_LABELED,
        $.ISINSTRICTFP
      ),
    varexp_argument: ($) =>
      choice(
        $.sortId,
        seq($.TYPEOF, $.LPAREN, field("y", $.varId), $.RPAREN),
        seq($.CONTAINERTYPE, $.LPAREN, field("y", $.varId), $.RPAREN),
        seq($.DEPENDINGON, $.LPAREN, field("y", $.varId), $.RPAREN),
        $.term
      ),
    goalspecs: ($) =>
      choice(
        $.CLOSEGOAL,
        seq($.goalspecwithoption, repeat(seq($.SEMI, $.goalspecwithoption)))
      ),
    goalspecwithoption: ($) =>
      choice(
        seq(field("soc", $.option_list), $.LBRACE, $.goalspec, $.RBRACE),
        $.goalspec
      ),
    option: ($) => seq(field("cat", $.IDENT), $.COLON, field("value", $.IDENT)),
    option_list: ($) =>
      seq(
        $.LPAREN,
        choice(seq($.option, repeat(seq($.COMMA, $.option))), $.option_expr),
        $.RPAREN
      ),
    option_expr: ($) =>
      prec(
        101,
        choice(
          prec.left(seq($.option_expr, $.AND, $.option_expr)),
          prec.left(seq($.option_expr, $.OR, $.option_expr)),
          seq($.NOT, $.option_expr),
          seq($.LPAREN, $.option_expr, $.RPAREN),
          $.option
        )
      ),
    goalspec: ($) =>
      seq(
        optional(seq(field("name", $.string_value), $.COLON)),
        choice(
          seq(
            field("rwObj", $.replacewith),
            optional(field("addSeq", $.add)),
            optional(field("addRList", $.addrules)),
            optional(field("addpv", $.addprogvar))
          ),
          seq(field("addSeq", $.add), optional(field("addRList", $.addrules))),
          field("addRList", $.addrules)
        )
      ),
    replacewith: ($) =>
      seq($.REPLACEWITH, $.LPAREN, field("o", $.termorseq), $.RPAREN),
    add: ($) => seq($.ADD, $.LPAREN, field("s", $.seq), $.RPAREN),
    addrules: ($) =>
      seq($.ADDRULES, $.LPAREN, field("lor", $.tacletlist), $.RPAREN),
    addprogvar: ($) =>
      seq($.ADDPROGVARS, $.LPAREN, field("pvs", $.pvset), $.RPAREN),
    tacletlist: ($) => seq($.taclet, repeat(seq($.COMMA, $.taclet))),
    pvset: ($) => seq($.varId, repeat(seq($.COMMA, $.varId))),
    rulesets: ($) =>
      seq(
        $.HEURISTICS,
        $.LPAREN,
        $.ruleset,
        repeat(seq($.COMMA, $.ruleset)),
        $.RPAREN
      ),
    ruleset: ($) => field("id", $.IDENT),
    metaId: ($) => field("id", $.simple_ident),
    metaTerm: ($) =>
      seq(
        field("vf", $.metaId),
        optional(
          seq(
            $.LPAREN,
            field("t", $.term),
            repeat(seq($.COMMA, field("t", $.term))),
            $.RPAREN
          )
        )
      ),
    contracts: ($) =>
      seq($.CONTRACTS, $.LBRACE, repeat($.one_contract), $.RBRACE),
    invariants: ($) =>
      seq(
        $.INVARIANTS,
        $.LPAREN,
        field("selfVar", $.one_bound_variable),
        $.RPAREN,
        $.LBRACE,
        repeat($.one_invariant),
        $.RBRACE
      ),
    one_contract: ($) =>
      seq(
        field("contractName", $.simple_ident),
        $.LBRACE,
        optional($.prog_var_decls),
        field("fma", $.term),
        $.MODIFIES,
        field("modifiesClause", $.term),
        $.RBRACE,
        $.SEMI
      ),
    one_invariant: ($) =>
      seq(
        field("invName", $.simple_ident),
        $.LBRACE,
        field("fma", $.term),
        optional(seq($.DISPLAYNAME, field("displayName", $.string_value))),
        $.RBRACE,
        $.SEMI
      ),
    rulesOrAxioms: ($) =>
      prec(
        100,
        seq(
          optional(field("doc", $.DOC_COMMENT)),
          choice($.RULES, $.AXIOMS),
          optional(field("choices", $.option_list)),
          seq($.LBRACE, repeat(seq(field("s", $.taclet), $.SEMI)), $.RBRACE)
        )
      ),
    bootClassPath: ($) =>
      seq($.BOOTCLASSPATH, field("id", $.string_value), $.SEMI),
    classPaths: ($) =>
      seq(
        $.CLASSPATH,
        field("s", $.string_value),
        repeat(seq($.COMMA, field("s", $.string_value))),
        $.SEMI
      ),
    string_literal: ($) => field("id", $.STRING_LITERAL),
    string_value: ($) => $.STRING_LITERAL,
    javaSource: ($) =>
      seq($.JAVASOURCE, field("result", $.oneJavaSource), $.SEMI),
    oneJavaSource: ($) => repeat1(choice($.string_value, $.COLON)),
    profile: ($) => seq($.PROFILE, field("name", $.string_value), $.SEMI),
    preferences: ($) =>
      seq(
        $.KEYSETTINGS,
        $.LBRACE,
        optional(field("s", $.string_value)),
        $.RBRACE
      ),
    proofScript: ($) => seq($.PROOFSCRIPT, field("ps", $.STRING_LITERAL)),
    proof: ($) => seq($.PROOF),

    //

    SORTS: ($) => "\\sorts",
    GENERIC: ($) => "\\generic",
    PROXY: ($) => "\\proxy",
    EXTENDS: ($) => "\\extends",
    ONEOF: ($) => "\\oneof",
    ABSTRACT: ($) => "\\abstract",
    SCHEMAVARIABLES: ($) => "\\schemaVariables",
    SCHEMAVAR: ($) => "\\schemaVar",
    MODALOPERATOR: ($) => "\\modalOperator",
    PROGRAM: ($) => "\\program",
    FORMULA: ($) => "\\formula",
    TERM: ($) => "\\term",
    UPDATE: ($) => "\\update",
    VARIABLES: ($) => "\\variables",
    VARIABLE: ($) => "\\variable",
    SKOLEMTERM: ($) => "\\skolemTerm",
    SKOLEMFORMULA: ($) => "\\skolemFormula",
    TERMLABEL: ($) => "\\termlabel",
    MODIFIES: ($) => "\\modifies",
    PROGRAMVARIABLES: ($) => "\\programVariables",
    STORE_TERM_IN: ($) => "\\storeTermIn",
    STORE_STMT_IN: ($) => "\\storeStmtIn",
    HAS_INVARIANT: ($) => "\\hasInvariant",
    GET_INVARIANT: ($) => "\\getInvariant",
    GET_FREE_INVARIANT: ($) => "\\getFreeInvariant",
    GET_VARIANT: ($) => "\\getVariant",
    IS_LABELED: ($) => "\\isLabeled",
    SAME_OBSERVER: ($) => "\\sameObserver",
    VARCOND: ($) => "\\varcond",
    APPLY_UPDATE_ON_RIGID: ($) => "\\applyUpdateOnRigid",
    DEPENDINGON: ($) => "\\dependingOn",
    DISJOINTMODULONULL: ($) => "\\disjointModuloNull",
    DROP_EFFECTLESS_ELEMENTARIES: ($) => "\\dropEffectlessElementaries",
    DROP_EFFECTLESS_STORES: ($) => "\\dropEffectlessStores",
    SIMPLIFY_IF_THEN_ELSE_UPDATE: ($) => "\\simplifyIfThenElseUpdate",
    ENUM_CONST: ($) => "\\enumConstant",
    FREELABELIN: ($) => "\\freeLabelIn",
    HASSORT: ($) => "\\hasSort",
    FIELDTYPE: ($) => "\\fieldType",
    FINAL: ($) => "\\final",
    ELEMSORT: ($) => "\\elemSort",
    HASLABEL: ($) => "\\hasLabel",
    HASSUBFORMULAS: ($) => "\\hasSubFormulas",
    ISARRAY: ($) => "\\isArray",
    ISARRAYLENGTH: ($) => "\\isArrayLength",
    ISCONSTANT: ($) => "\\isConstant",
    ISENUMTYPE: ($) => "\\isEnumType",
    ISINDUCTVAR: ($) => "\\isInductVar",
    ISLOCALVARIABLE: ($) => "\\isLocalVariable",
    ISOBSERVER: ($) => "\\isObserver",
    DIFFERENT: ($) => "\\different",
    METADISJOINT: ($) => "\\metaDisjoint",
    ISTHISREFERENCE: ($) => "\\isThisReference",
    DIFFERENTFIELDS: ($) => "\\differentFields",
    ISREFERENCE: ($) => "\\isReference",
    ISREFERENCEARRAY: ($) => "\\isReferenceArray",
    ISSTATICFIELD: ($) => "\\isStaticField",
    ISINSTRICTFP: ($) => "\\isInStrictFp",
    ISSUBTYPE: ($) => "\\sub",
    EQUAL_UNIQUE: ($) => "\\equalUnique",
    NEW: ($) => "\\new",
    NEW_TYPE_OF: ($) => "\\newTypeOf",
    NEW_DEPENDING_ON: ($) => "\\newDependingOn",
    HAS_ELEMENTARY_SORT: ($) => "\\hasElementarySort",
    NEWLABEL: ($) => "\\newLabel",
    CONTAINS_ASSIGNMENT: ($) => "\\containsAssignment",
    NOT_: ($) => "\\not",
    NOTFREEIN: ($) => "\\notFreeIn",
    SAME: ($) => "\\same",
    STATIC: ($) => "\\static",
    STATICMETHODREFERENCE: ($) => "\\staticMethodReference",
    MAXEXPANDMETHOD: ($) => "\\mayExpandMethod",
    STRICT: ($) => "\\strict",
    TYPEOF: ($) => "\\typeof",
    INSTANTIATE_GENERIC: ($) => "\\instantiateGeneric",

    FORALL: ($) => token(choice("\\forall", "\u2200")),
    EXISTS: ($) => token(choice("\\exists", "\u2203")),
    SUBST: ($) => "\\subst",
    IF: ($) => "\\if",
    IFEX: ($) => "\\ifEx",
    THEN: ($) => "\\then",
    ELSE: ($) => "\\else",
    INCLUDE: ($) => "\\include",
    INCLUDELDTS: ($) => "\\includeLDTs",
    CLASSPATH: ($) => "\\classpath",
    BOOTCLASSPATH: ($) => "\\bootclasspath",
    NODEFAULTCLASSES: ($) => "\\noDefaultClasses",

    JAVASOURCE: ($) => "\\javaSource",

    WITHOPTIONS: ($) => "\\withOptions",

    OPTIONSDECL: ($) => "\\optionsDecl",

    KEYSETTINGS: ($) => "\\settings",

    PROFILE: ($) => "\\profile",

    TRUE: ($) => "true",

    FALSE: ($) => "false",

    SAMEUPDATELEVEL: ($) => "\\sameUpdateLevel",

    INSEQUENTSTATE: ($) => "\\inSequentState",

    ANTECEDENTPOLARITY: ($) => "\\antecedentPolarity",

    SUCCEDENTPOLARITY: ($) => "\\succedentPolarity",

    CLOSEGOAL: ($) => "\\closegoal",

    HEURISTICSDECL: ($) => "\\heuristicsDecl",

    NONINTERACTIVE: ($) => "\\noninteractive",

    DISPLAYNAME: ($) => "\\displayname",

    HELPTEXT: ($) => "\\helptext",

    REPLACEWITH: ($) => "\\replacewith",

    ADDRULES: ($) => "\\addrules",

    ADDPROGVARS: ($) => "\\addprogvars",

    HEURISTICS: ($) => "\\heuristics",

    FIND: ($) => "\\find",

    ADD: ($) => "\\add",

    ASSUMES: ($) => "\\assumes",

    TRIGGER: ($) => "\\trigger",

    AVOID: ($) => "\\avoid",

    PREDICATES: ($) => "\\predicates",

    FUNCTIONS: ($) => "\\functions",

    TRANSFORMERS: ($) => "\\transformers",

    UNIQUE: ($) => "\\unique",

    RULES: ($) => "\\rules",

    AXIOMS: ($) => "\\axioms",

    PROBLEM: ($) => "\\problem",

    CHOOSECONTRACT: ($) => "\\chooseContract",

    PROOFOBLIGATION: ($) => "\\proofObligation",

    PROOF: ($) => "\\proof",

    PROOFSCRIPT: ($) => "\\proofScript",

    CONTRACTS: ($) => "\\contracts",

    INVARIANTS: ($) => "\\invariants",

    LEMMA: ($) => "\\lemma",

    IN_TYPE: ($) => "\\inType",

    IS_ABSTRACT_OR_INTERFACE: ($) => "\\isAbstractOrInterface",

    CONTAINERTYPE: ($) => "\\containerType",

    UTF_PRECEDES: ($) => "\u227A",

    UTF_IN: ($) => "\u220A",

    UTF_EMPTY: ($) => "\u2205",

    UTF_UNION: ($) => "\u222A",

    UTF_INTERSECT: ($) => "\u2229",

    UTF_SUBSET: ($) => "\u2286",

    UTF_SETMINUS: ($) => "\u2216",

    SEMI: ($) => ";",

    SLASH: ($) => "/",

    COLON: ($) => ":",

    DOUBLECOLON: ($) => "::",

    ASSIGN: ($) => ":=",

    DOT: ($) => ".",
    DOTRANGE: ($) => "..",
    COMMA: ($) => ",",

    LPAREN: ($) => "(",

    RPAREN: ($) => ")",

    LBRACE: ($) => "{",

    RBRACE: ($) => "}",

    LBRACKET: ($) => "[",

    RBRACKET: ($) => "]",

    EMPTYBRACKETS: ($) => "[]",

    AT: ($) => "@",

    PARALLEL: ($) => "||",

    OR: ($) => /[|\u2228]/,

    AND: ($) => /&|\u2227/,

    NOT: ($) => /!|\u00AC/,

    IMP: ($) => /->|\u2192/,

    EQUALS: ($) => "=",
    NOT_EQUALS: ($) => /!=|\u2260/,
    SEQARROW: ($) => /==>|\u27F9/,
    EXP: ($) => "^",
    TILDE: ($) => "~",
    PERCENT: ($) => "%",
    STAR: ($) => "*",
    MINUS: ($) => "-",
    PLUS: ($) => "+",
    GREATER: ($) => ">",
    GREATEREQUAL: ($) => /(>=)|(\u2265)/,
    RGUILLEMETS: ($) => ">>",
    STRING_LITERAL: ($) => /".*?"/,
    LESS: ($) => "<",
    LESSEQUAL: ($) => /<=|\u2264/,
    LGUILLEMETS: ($) => "<<",
    IMPLICIT_IDENT: ($) => /<[a-zA-Z]*>/,
    EQV: ($) => /<->\u2194/,
    CHAR_LITERAL: ($) => /'.'/,
    QUOTED_STRING_LITERAL: ($) => /".*?"/,
    SL_COMMENT: ($) => /\/\/.*?\n/,
    DOC_COMMENT: ($) => /\/\*!.*\*\//,
    ML_COMMENT: ($) => /\/\*.*?\*\//,
    BIN_LITERAL: ($) => /0b[0-9_]+/,
    HEX_LITERAL: ($) => /0x[0-9_]+/,
    INT_LITERAL: ($) => /[0123456789]+/,
    IDENT: ($) => /[a-zA-Z_][a-zA-Z_0-9]/,
    _RATIONAL_LITERAL: ($) =>
      token(
        seq(optional(/[0-9]/), ".", /[0-9]/, optional(seq(/[eE]/, /[0-9]/)))
      ),
    FLOAT_LITERAL: ($) => seq($._RATIONAL_LITERAL, /[fF]/),
    DOUBLE_LITERAL: ($) => seq($._RATIONAL_LITERAL, /[dD]/),
    REAL_LITERAL: ($) => seq($._RATIONAL_LITERAL, /[rR]/),

    MODALITY: ($) =>
      token(
        choice(
          seq("\\<", /.*?/, "\\>"),
          seq("\\[", /.*?/, "\\]"),
          seq("\\[[", /.*?/, "\\]]"),
          seq(
            choice(
              "\\box",
              "\\diamond",
              "\\diamond_transaction",
              "\\modality",
              "\\box_transaction",
              "\\throughout",
              "\\throughout_transaction"
            ),
            /.*?/,
            "\\endmodality"
          )
        )
      ),
  },
};

module.exports = grammar(the_grammar);
