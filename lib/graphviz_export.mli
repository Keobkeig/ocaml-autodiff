(** Graph export scaffolding for computation/expression visualization. *)
type dot_graph_builder = {
  mutable counter : int;
  mutable nodes : (int * string) list;
  mutable edges : (int * int) list;
  vars : (string, int) Hashtbl.t;
}
(** AF: A value of type [dot_graph_builder] represents an expression graph with
    an association list [nodes] mapping ids to labels, edgeset [edges] mapping
    the tail id to the head id. It also keeps track of variables in the graph as
    [vars]. An internal [counter] is used for assigning the ids.

    RI: Node ids must be positive integers, and no duplicates are allowed in
    [vars]. [counter] must not exceed the integer limit for proper usage. *)

val make_graph : unit -> dot_graph_builder
(** [make_graph] initializes an [dot_graph_builder] that represents an empty
    graph*)

val generate_id : dot_graph_builder -> int
(** [generate_id builder] Generates a fresh id from [b]. *)

val to_graph : dot_graph_builder -> Expr.t -> int
(** [to_graph builder expr] recursively populates [builder] based on [expr]. The
    value returned is the id assigned to the deepest node in the expression
    graph that represents [expr]. Primarily used as a helper for [expr_to_dot].*)

val expr_to_dot : Expr.t -> string
(** [expr_to_dot expr] returns a DOT graph string for the expression AST. This
    is a minimal scaffold output for Sprint-B and beyond. *)

val write_expr_dot : string -> Expr.t -> (unit, string) result
(** [write_expr_dot path expr] writes a DOT graph file for [expr] at [path]. *)
